;;; evil-ts-obj.el --- Provides evil text-objects using tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-treesit-objects
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides evil text-objects using tree-sitter
;;
;;; Code:

(require 'treesit)
(require 'evil)

(require 'evil-ts-obj-conf)
(require 'evil-ts-obj-common)

(defun evil-ts-obj--root-at (pos)
  "Return root node at the given `POS'."
  (or (when-let ((parser
                  (car (treesit-local-parsers-at pos))))
        (treesit-parser-root-node parser))
      (treesit-buffer-root-node (treesit-language-at pos))))

(defun evil-ts-obj--node-at-or-around (pos)
  "Return leaf node, which is at or near `POS'.
If `POS' is inside some leaf node (node-start <= pos < node-end),
then return this node. If `POS' is on a whitespace, examine
previous and next nodes that are on the same line. Prefer named
nodes over anonymous ones. If both nodes are named select the
next one. If both nodes are anonymous prefer node, which text is
equal to `evil-ts-obj-conf-param-sep' otherwise the next one. If
the pos is on `evil-ts-obj-conf-param-sep' select the previous
named node, if it exists. Return nil if no node can be found."

  (let (prefer-previous)
    (if (or (memq (char-after pos) '(32 9 10 nil))
            (and (= (char-after pos) (string-to-char evil-ts-obj-conf-param-sep))
                 (setq prefer-previous t)))

        ;; special case:
        ;; point is either on space or on a separator
        (let* ((prev-pos (save-excursion
                           (goto-char pos)
                           (skip-chars-backward " \t")
                           (and (not (bolp)) (1- (point)))))

               (next-pos (save-excursion
                           (goto-char pos)
                           (skip-chars-forward " \t")
                           (and (not (eolp)) (point))))
               node
               next-node
               next-named
               prev-node)

          (when next-pos
            (setq next-node (treesit-node-at next-pos))
            (when (and (not prefer-previous)
                       (setq next-named (treesit-node-check next-node 'named)))
              (setq node next-node)))

          (when (and prev-pos
                     (null node))
            (setq prev-node (treesit-node-at prev-pos))
            (when (treesit-node-check prev-node 'named)
              (setq node prev-node)))

          (when (null node)
            (cond (next-named
                   ;; prefer-previous was t, but previous one was anonymous
                   (setq node next-node))

                  ;; no named nodes on both sides
                  ((equal (treesit-node-type prev-node) evil-ts-obj-conf-param-sep)
                   (setq node (evil-ts-obj--node-at-or-around (treesit-node-start prev-node))))
                  ((null next-node)
                   (setq node prev-node))
                  (t
                   (setq node next-node))))
          node)
      (treesit-node-at pos))))

(defun evil-ts-obj--smallest-node-at (pos &optional named)
  (let* ((root (evil-ts-obj--root-at pos))
         (node  (treesit-node-on pos pos nil named)))
    (when (not (treesit-node-eq node root))
      node)))

(defun evil-ts-obj--thing-around (pos thing &optional dont-step-forward)
  "Return the node that represents thing, which encloses `POS' or is near it.
`THING' should be a thing defined in `treesit-thing-settings'. If
there is no enclosing thing, jump to the next one unless
`DONT-STEP-FORWARD'. This function also handles the case when
multiple things start at the same position. e.g. in yaml - item1
Hyphen symbol is the beginning of the list and a list item. When
there are multiple overlapping things, the result depends on
`POS'. If `POS' is inside of any thing, then the smallest
enclosing thing is returned. If the `POS' is before any thing (on
the same line), function returns the largest node that starts
after `POS'. If the `POS' is after any thing, function returns
the largest node that ends before `POS'. It returns nil if no
thing can be found (e.g. empty line)."

  (let* ((cursor (evil-ts-obj--node-at-or-around pos))
         (iter-pred (lambda (node)
                      (treesit-node-match-p node thing t)))
         (enclosing-node (treesit-parent-until cursor iter-pred t))
         (before-cursor (and enclosing-node
                             (< pos (treesit-node-start enclosing-node))))
         (after-cursor (and enclosing-node
                            (<= (treesit-node-end enclosing-node) pos))))

    (message "cursor %s enclosing %s" cursor enclosing-node)
    (message "before %s after %s" before-cursor after-cursor)
    (cond
     (before-cursor
      (while (and
              (setq cursor (treesit-parent-until enclosing-node iter-pred))
              (when (= (treesit-node-start enclosing-node)
                       (treesit-node-start cursor))
                (setq enclosing-node cursor)))))
     (after-cursor
      (while (and
              (setq cursor (treesit-parent-until cursor iter-pred))
              (when (= (treesit-node-end enclosing-node)
                       (treesit-node-end cursor))
                (setq enclosing-node cursor))))))

    (if enclosing-node
        enclosing-node
      ;; try to find next node
      (unless dont-step-forward
        (treesit--thing-next (point) thing)))))

(defun evil-ts-obj--current-thing (node nav-thing)
  "If `NAV-THING' is symbol or string it is returned as is.
If it is list, it is considered to be in the form (or <thing1>
<thing2> &rest). In this case, all things are tried sequentially
and the first that matches against `NODE' is returned."
  (pcase nav-thing
    ((or (pred symbolp)
         (pred stringp))
     nav-thing)
    ((pred listp)
     (seq-find (lambda (thing) (treesit-node-match-p node thing t))
               (cdr nav-thing) nil))
    (_ (error "Unsupported thing %s" nav-thing))))

(defun evil-ts-obj--make-spec (op-kind &optional thing text-obj command)
  `(:thing ,thing
    :text-obj ,text-obj
    :op-kind ,op-kind
    :command ,(or command
                  evil-this-operator
                  (and (bound-and-true-p avy-action)
                       (not (eq avy-action #'identity))
                       avy-action)
                  this-command)))

(defun evil-ts-obj--default-range (node spec)
  "Return default text object range for a `NODE' based on `SPEC'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (pcase spec
      ((pmap (:text-obj 'upper))
       (let ((final-sibling node))
         (while (setq node (treesit-node-prev-sibling node t))
           (setq final-sibling node))
         (setq start (treesit-node-start final-sibling))))
      ((pmap (:text-obj 'lower))
       (let ((final-sibling node))
         (while (setq node (treesit-node-next-sibling node t))
           (setq final-sibling node))
         (setq end (treesit-node-end final-sibling)))))
    (list start end)))

(defun evil-ts-obj--apply-modifiers (node pos thing spec)
  "Apply modifiers for creating a text object from the `THING'.
`THING' that is represented by `NODE' is transformed to range
via transformation function from
`evil-ts-obj-conf-thing-modifiers'. `POS' is used to determine
current language. Modifier is chosen based on `SPEC'. If no
modifier is found use `evil-ts-obj--default-range'."
  (when node
    (if-let* ((modifier (plist-get evil-ts-obj-conf-thing-modifiers
                                   (treesit-language-at pos)))
              (current-thing (evil-ts-obj--current-thing node thing))
              (spec (plist-put spec :thing current-thing))
              (range (funcall modifier spec node)))
        range
      (evil-ts-obj--default-range node spec))))


(defun evil-ts-obj--get-text-obj-range (pos thing spec &optional return-node)
  "Return a range for text object described via `SPEC'.
Create text object using `THING' on position `POS'. If
`RETURN-NODE' is t return treesit node as a last item of the
list."
  (when-let ((node (evil-ts-obj--thing-around pos thing)))
    (append
     (evil-ts-obj--apply-modifiers node pos thing spec)
     (when return-node (list node)))))


;; * Movement

(defun evil-ts-obj--begin-of-thing (thing)
  "Determine `THING' at point and move point to the beginning of it.
Beginning position is calculated based on spec with op-kind set
to nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
beginning, move to the beginning of the parent thing."
  (when-let* ((spec (evil-ts-obj--make-spec 'nav))
              (range (evil-ts-obj--get-text-obj-range (point) thing spec t))
              (node (car (last range))))
    (while (and range
                (<= (point) (car range)))
      ;; jump to beginning of a parent.
      ;; Take into consideration that parent may start on the same position as the child.
      (if-let ((parent-node (treesit-parent-until
                             node (lambda (n) (treesit-node-match-p n thing t)))))
          (setq node parent-node
                range (evil-ts-obj--apply-modifiers
                       parent-node (treesit-node-start parent-node) thing spec))
        (setq range nil)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--end-of-thing (thing)
  "Determine `THING' at point and move point to the end of it.
End position is calculated based on spec with op-kind set to
nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
end, move to the end of the parent thing."
  (when-let ((spec (evil-ts-obj--make-spec 'nav))
             (range (evil-ts-obj--get-text-obj-range (point) thing spec t))
             (node (car (last range))))
    (while (and range
                (<= (1- (cadr range)) (point)))
      (if-let (parent-node (treesit-parent-until
                            node (lambda (n) (treesit-node-match-p n thing t))))
          (setq node parent-node
                range (evil-ts-obj--apply-modifiers
                       parent-node (treesit-node-end parent-node) thing spec))
        (setq range nil)))

    (when range
      (goto-char (1- (cadr range))))))



(defun evil-ts-obj--jump-boundaries ()
  (interactive)
  (when-let* ((thing evil-ts-obj-conf-nav-thing)
              (node (evil-ts-obj--thing-around (point) thing)))
    (cond
     ((= (treesit-node-start node) (point)) (goto-char (1- (treesit-node-end node))))
     ((= (1- (treesit-node-end node)) (point)) (goto-char (treesit-node-start node)))
     (t (goto-char (treesit-node-end node))))))


(defun evil-ts-obj--maybe-set-jump (init-enclosing-node node init-pos)
  (when (or (not (equal (treesit-node-parent init-enclosing-node)
                        (treesit-node-parent node)))
            (and init-enclosing-node
                 (/= (treesit-node-start init-enclosing-node)
                     init-pos)))
    ;; we step up one or more times to parent,
    ;; so it won't be use to return to previous position.
    ;; Save position to jump list
    (evil-set-jump)))

(defun evil-ts-obj--find-matching-sibling (thing init-node &optional prev)
  (let ((node init-node))
    (catch 'break
      (while (setq node (if prev
                            (treesit-node-prev-sibling node t)
                          (treesit-node-next-sibling node t)))
        (when (treesit-node-match-p node thing t)
          (throw 'break node))))))

(defun evil-ts-obj--next-thing (thing init-enclosing-node init-pos)
  "Return node that represents the next `THING'.
When `INIT-ENCLOSING-NODE' is not nil, it is considered to be
a node that represents current thing. Next thing should be
associated with the node that is not equal to
`INIT-ENCLOSING-NODE' and that starts after `INIT-POS'."

  (let ((parent init-enclosing-node)
        (pos (treesit-node-end init-enclosing-node))
        next)
    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move forward
        (setq next (treesit--thing-next init-pos thing))

      ;; go to parent and try to move to next sibling from there
      (while (and (or (null next)
                      (= init-pos (treesit-node-start next)))
                  parent
                  (< pos (point-max)))
        (setq next (treesit--thing-next pos thing)
              parent (treesit-node-parent parent)
              pos (treesit-node-end parent))))
    next))

(defun evil-ts-obj--goto-next-largest-thing (thing)
  "Go to the next `THING'.
At first, determine current THING at point. After that move to
the next largest `THING' that starts after the current thing
ends."
  (let* ((spec (evil-ts-obj--make-spec 'nav))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         next)

    (setq next (evil-ts-obj--next-thing thing init-enclosing-node init-pos))

    (when-let ((node next)
               (range (evil-ts-obj--apply-modifiers
                       node (treesit-node-start node) thing spec)))
      (evil-ts-obj--maybe-set-jump init-enclosing-node node init-pos)
      (goto-char (car range)))))



(defun evil-ts-obj--goto-prev-largest-thing (thing)
  "Go to the previous `THING'.
At first, determine current THING at point. After that move to
the `THING' that ends before the current thing starts. If no such
a THING exists jump to a parent THING."
  (let* ((spec (evil-ts-obj--make-spec 'nav))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing))
         (parent init-enclosing-node)
         (pos (treesit-node-start init-enclosing-node))
         prev range)

    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move backward
        (setq prev (treesit--thing-prev init-pos thing)
              range (evil-ts-obj--apply-modifiers prev (treesit-node-start prev) thing spec))

      (while (and (or (null range)
                      (equal init-enclosing-node prev)
                      ;; have to find a range that will move (<-) us from the
                      ;; current position
                      (<= init-pos (car range)))
                  (< (point-min) pos))
        ;; trying to get previous thing
        (setq prev (treesit--thing-prev pos thing))

        (when (null prev)
          ;; try to step up till we move from the start position of current enclosing node
          ;; make one step per iteration
          (setq parent (treesit-parent-until
                        parent (lambda (n) (treesit-node-match-p n thing t)))
                prev parent
                pos (or (treesit-node-start prev) 0)))


        (setq range (evil-ts-obj--apply-modifiers prev pos thing spec))))


    (when range
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--get-nav-thing (&optional current)
  (let ((thing (or evil-ts-obj-conf-nav-thing
                   'compound)))
    ;; try to guess to what thing to move
    (when-let* (current
                (nav-thing evil-ts-obj-conf-nav-thing)
                (node (evil-ts-obj--thing-around (point) nav-thing))
                (cur-thing (evil-ts-obj--current-thing node nav-thing)))
      (setq thing cur-thing))
    thing))



(defun evil-ts-obj--search-subtree (node thing child-filter &optional backward match-fn)
  (let* ((filtered-children (seq-filter child-filter (treesit-node-children node t)))
         (children (if backward (nreverse filtered-children) filtered-children))
         (match-pred (or match-fn
                         (lambda (n) (and (funcall child-filter n)
                                          (treesit-node-match-p n thing t)))))
         result)

    (cl-loop for child in children
             do (setq result (treesit-search-subtree child match-pred backward))
             when result
             return result)))

(defun evil-ts-obj--search-subtree-forward (start-node thing init-pos)
  (let ((filter (lambda (c) (< init-pos (treesit-node-end c)))))
    (evil-ts-obj--search-subtree start-node thing filter)))

(defun evil-ts-obj--search-subtree-backward (start-node thing spec init-pos)
  (let ((filter (lambda (c) (< (treesit-node-start c) init-pos)))
        (match-pred (lambda (n) (and (treesit-node-match-p n thing t)
                                     (let ((range (evil-ts-obj--apply-modifiers
                                                   n (treesit-node-start n) thing spec)))
                                       (< (car range) init-pos)))))
        (node start-node)
        child)
    ;; have to find the deepest node for that subtree
    (while (setq node (evil-ts-obj--search-subtree node thing filter t match-pred))
      (setq child node))
    child))

(defun evil-ts-obj--find-next-thing (thing pos)
  (let ((enclosing-node (evil-ts-obj--smallest-node-at pos))
        (init-pos pos))
    (if-let ((node enclosing-node)
             (next (evil-ts-obj--search-subtree-forward node thing init-pos)))
        next
      (evil-ts-obj--next-thing thing enclosing-node pos))))

(defun evil-ts-obj--goto-next-thing (thing)
  (when-let* ((spec (evil-ts-obj--make-spec 'nav))
              (init-pos (point))
              (next (evil-ts-obj--find-next-thing thing init-pos))
              (range (evil-ts-obj--apply-modifiers
                      next (treesit-node-start next) thing spec)))
    (goto-char (car range))))

(defun evil-ts-obj--goto-prev-thing (thing)
  (let* ((spec (evil-ts-obj--make-spec 'nav))
         (init-pos (point))
         (enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         (cursor enclosing-node)
         range )

    ;; search backwardly inside current thing
    (if-let ((node enclosing-node)
             (prev (evil-ts-obj--search-subtree-backward node thing spec init-pos)))
        (setq range (evil-ts-obj--apply-modifiers
                     prev (treesit-node-start prev) thing spec))

      ;; no matching things inside the current thing
      ;; search backwardly starting from  enclosing node
      (while (and cursor
                  (or (null range)
                      ;; have to find a range that will move (<-) us from the
                      ;; current position
                      (<= init-pos (car range))))

        (setq prev (treesit-search-forward
                    cursor
                    (lambda (n) (treesit-node-match-p n thing t))
                    t)
              cursor prev
              range (evil-ts-obj--apply-modifiers
                     prev (treesit-node-start prev) thing spec))))


    (when range
      (goto-char (car range)))))



;; * interactive functions

(evil-define-motion evil-ts-obj-next-largest-thing (count)
  "Jump to the next sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (evil-without-repeat
      (let ((thing (evil-ts-obj--get-nav-thing)))
        (dotimes (_ (or count 1))
          (evil-ts-obj--goto-next-largest-thing thing))))))

(evil-define-motion evil-ts-obj-same-next-largest-thing (count)
  "Jump to the same next sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (evil-without-repeat
      (let ((thing (evil-ts-obj--get-nav-thing t)))
        (dotimes (_ (or count 1))
          (evil-ts-obj--goto-next-largest-thing thing))))))

(evil-define-motion evil-ts-obj-previous-largest-thing (count)
  "Jump to the previous sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-largest-thing thing)))))

(evil-define-motion evil-ts-obj-same-previous-largest-thing (count)
  "Jump to the same previous sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-largest-thing thing)))))


(evil-define-motion evil-ts-obj-beginning-of-thing (count)
  "Jump to the beginning of the current thing."
  :type inclusive
  :jump t
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--begin-of-thing thing)))))

(evil-define-motion evil-ts-obj-end-of-thing (count)
  "Jump to the end of the current thing."
  :type inclusive
  :jump t
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--end-of-thing thing)))))

(evil-define-motion evil-ts-obj-next-thing (count)
  "Jump to the next thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-next-thing thing)))))

(evil-define-motion evil-ts-obj-same-next-thing (count)
  "Jump to the same next thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-next-thing thing)))))

(evil-define-motion evil-ts-obj-previous-thing (count)
  "Jump to the previous thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-thing thing)))))

(evil-define-motion evil-ts-obj-same-previous-thing (count)
  "Jump to the same previous thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-thing thing)))))





(defmacro evil-ts-obj-define-text-obj (thing text-obj)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-%s-%s" thing text-obj))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       ,(format "Select a %s %s object." thing text-obj)
       (evil-ts-obj--get-text-obj-range (point) ',thing
                                     (evil-ts-obj--make-spec 'mod ',thing ',text-obj)))))

(defun evil-ts-obj--finalize-compound (range)
  (pcase-let ((`(,first-pos ,last-pos) range))
    (when (eq (char-before last-pos) ?\n)
      (setq last-pos (1- last-pos)))
    (list first-pos last-pos)))

(evil-define-text-object evil-ts-obj-compound-outer (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--finalize-compound
   (evil-ts-obj--get-text-obj-range (point) 'compound
                                 (evil-ts-obj--make-spec 'mod 'compound 'outer))))

(evil-ts-obj-define-text-obj compound inner)
(evil-ts-obj-define-text-obj compound upper)
(evil-ts-obj-define-text-obj compound lower)


(evil-ts-obj-define-text-obj statement outer)
(evil-ts-obj-define-text-obj statement inner)
(evil-ts-obj-define-text-obj statement upper)
(evil-ts-obj-define-text-obj statement lower)


(evil-ts-obj-define-text-obj param outer)
(evil-ts-obj-define-text-obj param inner)
(evil-ts-obj-define-text-obj param upper)
(evil-ts-obj-define-text-obj param lower)



;;* default keybindings and minor mode

(defvar evil-ts-obj-inner-text-objects-map
  (let ((map (make-sparse-keymap "Inner text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-inner)
    (define-key map (kbd "a") #'evil-ts-obj-param-inner)
    (define-key map (kbd "s") #'evil-ts-obj-statement-inner)
    map))
(defvar evil-ts-obj-outer-text-objects-map
  (let ((map (make-sparse-keymap "Outer text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-outer)
    (define-key map (kbd "a") #'evil-ts-obj-param-outer)
    (define-key map (kbd "s") #'evil-ts-obj-statement-outer)
    map))
(defvar evil-ts-obj-upper-text-objects-map
  (let ((map (make-sparse-keymap "Upper text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-upper)
    (define-key map (kbd "a") #'evil-ts-obj-param-upper)
    (define-key map (kbd "s") #'evil-ts-obj-statement-upper)
    map))
(defvar evil-ts-obj-lower-text-objects-map
  (let ((map (make-sparse-keymap "Lower text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-lower)
    (define-key map (kbd "a") #'evil-ts-obj-param-lower)
    (define-key map (kbd "s") #'evil-ts-obj-statement-lower)
    map))


;;;###autoload
(define-minor-mode evil-ts-obj-mode
  "A minor mode with tree sitter keybinds."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))


(evil-define-key '(visual operator) evil-ts-obj-mode-map
  "i" evil-ts-obj-inner-text-objects-map
  "a" evil-ts-obj-outer-text-objects-map
  "u" evil-ts-obj-upper-text-objects-map
  "o" evil-ts-obj-lower-text-objects-map)

(evil-define-key 'normal evil-ts-obj-mode-map
  (kbd "M-a") #'evil-ts-obj-beginning-of-thing
  (kbd "M-e") #'evil-ts-obj-end-of-thing
  (kbd "M-n") #'evil-ts-obj-next-largest-thing
  (kbd "C-M-n") #'evil-ts-obj-same-next-largest-thing
  (kbd "M-p") #'evil-ts-obj-previous-largest-thing
  (kbd "C-M-p") #'evil-ts-obj-same-previous-largest-thing
  (kbd "M-f") #'evil-ts-obj-next-thing
  (kbd "C-M-f") #'evil-ts-obj-same-next-thing
  (kbd "M-b") #'evil-ts-obj-previous-thing
  (kbd "C-M-b") #'evil-ts-obj-same-previous-thing)



(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
