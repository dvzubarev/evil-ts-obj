;;; evil-ts-obj-core.el --- Provides evil text-objects using tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-ts-obj
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
(require 'evil-ts-obj-util)


(defvar-local evil-ts-obj--last-text-obj-spec nil)
(defvar-local evil-ts-obj--last-text-obj-range nil)

(defun evil-ts-obj--root-at (pos)
  "Return root node at the given `POS'."
  (or (when-let ((parser
                  (car (treesit-local-parsers-at pos))))
        (treesit-parser-root-node parser))
      (treesit-buffer-root-node (treesit-language-at pos))))

(defun evil-ts-obj--text-bounds-at-pos (pos)
  (let ((syntax (char-to-string (char-syntax (char-after pos))))
        (start nil) (end nil))

    (save-excursion
      (goto-char pos)
      (skip-syntax-backward syntax)
      (setq start (point))
      (goto-char pos)
      (skip-syntax-forward syntax)
      (setq end (point)))
    (cons start end)))

(defun evil-ts-obj--find-next-prev-non-space (pos &optional end-pos)
  (let ((end-pos (or end-pos pos))
        (prev-pos nil)
        (next-pos nil))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t")
      (setq prev-pos
            (and (not (bolp)) (1- (point))))

      (goto-char end-pos)
      (skip-chars-forward " \t")
      (setq next-pos
            (and (not (eolp))
                 (point))))
    (cons prev-pos next-pos)))

(defun evil-ts-obj--get-nearest-words-pos (pos sep-regex)

  (if (memq (char-after pos) '(32 9 10 nil))
      ;; pos at whitespace
      (pcase-let ((`(,prev-pos . ,next-pos) (evil-ts-obj--find-next-prev-non-space pos)))
        (list 'space prev-pos next-pos))

    (pcase-let* ((`(,cur-word-start . ,cur-word-end) (evil-ts-obj--text-bounds-at-pos pos))
                 (cur-word (buffer-substring-no-properties cur-word-start cur-word-end)))

      (if (and sep-regex (string-match-p sep-regex cur-word))
          (pcase-let ((`(,prev-pos . ,next-pos)
                       (evil-ts-obj--find-next-prev-non-space cur-word-start cur-word-end)))

            (list 'sep prev-pos next-pos))
        (list 'other nil nil)))))

(defun evil-ts-obj--node-at-or-around (pos)
  "Return leaf node, which is at or near `POS'.
If `POS' is inside some leaf node (node-start <= pos < node-end),
then return this node. If `POS' is on a whitespace, examine
previous and next nodes that are on the same line. Prefer named
nodes over anonymous ones. If both nodes are named select the
next one. If both nodes are anonymous prefer node, which text
matches against `evil-ts-obj-conf-sep-regexps' otherwise the
next one. If the pos is on a node that matches against
`evil-ts-obj-conf-sep-regexps' select the previous named
node, if it exists. Return nil if no node can be found."

  (pcase-let* ((sep-regex (plist-get evil-ts-obj-conf-sep-regexps
                                     (treesit-language-at pos)))
               (`(,pos-at ,prev-pos ,next-pos) (evil-ts-obj--get-nearest-words-pos pos sep-regex))
               (prefer-previous))

    (pcase pos-at
      ('other
       (treesit-node-at pos))
      ((or 'space 'sep)
       (setq prefer-previous (eq pos-at 'sep))
       (let (node
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
           (when (or prefer-previous
                     (treesit-node-check prev-node 'named))
             (setq node prev-node)))

         (when (null node)
           (cond (next-named
                  ;; prefer-previous was t, but previous one was null
                  (setq node next-node))

                 ;; no named nodes on both sides
                 ((and
                   prev-node sep-regex (string-match-p sep-regex (treesit-node-type prev-node)))
                  (setq node (evil-ts-obj--node-at-or-around (treesit-node-start prev-node))))
                 ((and
                   next-node sep-regex (string-match-p sep-regex (treesit-node-type next-node)))
                  (if prev-node
                      (setq node prev-node)
                    (setq node (evil-ts-obj--node-at-or-around (treesit-node-start next-node)))))
                 ((null next-node)
                  (setq node prev-node))
                 (t
                  (setq node next-node))))
         node)))))

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
multiple things start at the same position. For example, yaml:

- item1
- item2

 The first hyphen symbol is the beginning of the list and a list
item. When there are multiple overlapping things, the result
depends on `POS'. If `POS' is inside of any thing, then the
smallest enclosing thing is returned. If the `POS' is at the
start of multiple things or before any thing (on the same line),
function returns the largest node that starts at or after `POS'.
If the `POS' is after any thing, function returns the largest
node that ends before `POS'. It returns nil if no thing can be
found (e.g. empty line)."

  (let* ((cursor (evil-ts-obj--node-at-or-around pos))
         (iter-pred (lambda (node)
                      (treesit-node-match-p node thing t)))
         (enclosing-node (treesit-parent-until cursor iter-pred t))
         (before-cursor (and enclosing-node
                             (<= pos (treesit-node-start enclosing-node))))
         (after-cursor (and enclosing-node
                            (<= (treesit-node-end enclosing-node) pos))))

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

(defun evil-ts-obj--make-spec (thing &optional action mod command)
  "Create plist that describes the current text object.
`THING' may be symbol or alist. If it is symbol, it is just added
to spec as is. If it is alist then multiple specs are created,
for each element of `THING' alist. Alist should contain
pairs (thing . mod). If `ACTION' is nil, it is set to op. `MOD'
may be nil if ACTION is nav or it is specified in
THINGs alist. If `COMMAND' is nil the first non-nil from this
list is chosen: `evil-this-operator', `avy-action',
`this-command'. If `evil-visual-state-p' returns t, then keyword
:visual is put to spec with the value t."
  (let* ((action (or action 'op))
         (cmd (or command
                  evil-this-operator
                  (and (bound-and-true-p avy-action)
                       (not (eq avy-action #'identity))
                       avy-action)
                  this-command))
         (spec `(:thing ,thing
                 :mod ,mod
                 :act ,action
                 :command ,cmd
                 :visual ,(evil-visual-state-p))))
    (if (not (listp thing))
        spec
      (mapcar (pcase-lambda (`(,thing . ,mod))
                (let ((spec-copy (copy-sequence spec)))
                  (cl-callf plist-put spec-copy :thing thing)
                  (cl-callf plist-put spec-copy :mod mod)
                  (cons thing spec-copy)))
              thing))))

(defun  evil-ts-obj--make-nav-spec (thing &optional command)
  "Create spec for navigation action.
THING may be symbol or list. If it is list it is expected in the
form (or thing1 thing2 ...). COMMAND is the same as for
`evil-ts-obj--make-spec'."
  (if (not (listp thing))
      (evil-ts-obj--make-spec thing 'nav nil command)
    (evil-ts-obj--make-spec (mapcar (lambda (th) (cons th nil)) (cdr thing))
                            'nav
                            nil command)))

(defun evil-ts-obj--default-range (node spec)
  "Return default text object range for a `NODE' based on `SPEC'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))

    ;; If we collecting text objects for previewing of candidates,
    ;; we do not need to extend each thing to its full upper/lower ranges.
    (when-let (((not (eq (plist-get spec :act) 'nav)))
               (parent (treesit-node-parent node)))
      ;; default handling of upper/lower text objects

      (pcase spec
        ((pmap (:mod 'upper))
         (let ((final-sibling (treesit-node-child parent 0 t)))
           (setq start (treesit-node-start final-sibling))))
        ((pmap (:mod 'lower))
         (let ((final-sibling (treesit-node-child parent -1 t)))
           (setq end (treesit-node-end final-sibling))))
        ((pmap (:mod 'all))
         (let ((first-node (treesit-node-child parent 0 t))
               (last-node (treesit-node-child parent -1 t)))
           (setq start (treesit-node-start first-node)
                 end (treesit-node-end last-node))))))
    (list start end)))

(defun evil-ts-obj--apply-modifiers (node thing spec)
  "Apply modifiers for creating a text object from the `THING'.
THING may be a symbol or a list of multiple things. If thing is a
list then one thing that represents the NODE is kept. In case if
THING is symbol, SPEC is a plist that contains all context for
the current operation, otherwise it is alist that maps thing to a
spec. `THING' is transformed to range via transformation function
from `evil-ts-obj-conf-thing-modifiers'. NODE and SPEC are passed
to modifiers. If no modifier is found or modifier returns nil
fallback to `evil-ts-obj--default-range'."
  (when node
    (let* ((current-thing (evil-ts-obj--current-thing node thing))
           (current-spec (if (listp thing)
                             (alist-get current-thing spec)
                           spec))
           (range
            (if-let* ((pos (treesit-node-start node))
                      (modifier (plist-get evil-ts-obj-conf-thing-modifiers
                                           (treesit-language-at pos)))
                      (r (funcall modifier current-spec node)))
                r
              (evil-ts-obj--default-range node current-spec))))
      (setq evil-ts-obj--last-text-obj-spec current-spec
            evil-ts-obj--last-text-obj-range (copy-sequence range))
      range)))


(defun evil-ts-obj--expand-active-region? (thing spec)
  (when-let* (((not (listp thing)))       ;multiple things are not supported
              ((plist-get spec :visual))
              ((plist-get evil-ts-obj--last-text-obj-spec :visual))
              ((eq (plist-get spec :mod)
                   (plist-get evil-ts-obj--last-text-obj-spec :mod)))
              (start (region-beginning))
              (end (1+ (region-end)))
              (cur-range (list start end))
              ((equal cur-range evil-ts-obj--last-text-obj-range)))
    ;; Selected the same text object in the visual state.
    cur-range))

(defun evil-ts-obj--get-text-obj-range (pos-or-node thing spec &optional extend-over-range)
  "Return a range for text object described via `SPEC'.
More information about `SPEC' and `THING' is in
`evil-ts-obj--apply-modifiers' documentation. If `POS-OR-NODE' is
not treesit-node, then it is considered to be position in the
buffer. In that case, find `THING' that is around this position
using `evil-ts-obj--thing-around'. If `EXTEND-OVER-RANGE' is not
nil or region is active and the last returned range is equal to
the active region, expand that region. It is done by searching
for a matching parent of the current thing. Otherwise apply range
modifiers to the found thing and return range of a text-object."

  (when-let ((node (if (treesit-node-p pos-or-node)
                       pos-or-node
                     (evil-ts-obj--thing-around pos-or-node thing)))
             (range (evil-ts-obj--apply-modifiers node thing spec)))

    (if-let* ((bounds (or extend-over-range
                          (evil-ts-obj--expand-active-region? thing spec)))
              (bound-start (car bounds))
              (bound-end (cadr bounds))
              (cur-range range))
        ;; Extend to the parent thing.
        (progn
          (while (and cur-range
                      (<= bound-start (car cur-range))
                      (<= (cadr cur-range) bound-end))
            (setq node (treesit-parent-until
                        node (lambda (n) (treesit-node-match-p n thing t)))
                  cur-range (evil-ts-obj--apply-modifiers node thing spec)))
          cur-range)
      range)))


;; * Movement

(defun evil-ts-obj--goto-begin-of-thing (thing)
  "Determine `THING' at point and move point to the beginning of it.
Beginning position is calculated based on spec with :act set
to nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
beginning, move to the beginning of the parent thing."
  (when-let* ((spec (evil-ts-obj--make-nav-spec thing))
              (node (evil-ts-obj--thing-around (point) thing))
              (range (evil-ts-obj--apply-modifiers node thing spec)))
    (while (and range
                (<= (point) (car range)))
      ;; jump to beginning of a parent.
      ;; Take into consideration that parent may start on the same position as the child.
      (if-let ((parent-node (treesit-parent-until
                             node (lambda (n) (treesit-node-match-p n thing t)))))
          (setq node parent-node
                range (evil-ts-obj--apply-modifiers parent-node thing spec))
        (setq range nil)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--goto-end-of-thing (thing)
  "Determine `THING' at point and move point to the end of it.
End position is calculated based on spec with :act set to
nav, so all nav modifiers affect it (see
`evil-ts-obj-conf-thing-modifiers'). If point is already at the
end, move to the end of the parent thing."
  (when-let* ((spec (evil-ts-obj--make-nav-spec thing))
              (node (evil-ts-obj--thing-around (point) thing))
              (range (evil-ts-obj--apply-modifiers node thing spec)))
    (while (and range
                (<= (1- (cadr range)) (point)))
      (if-let (parent-node (treesit-parent-until
                            node (lambda (n) (treesit-node-match-p n thing t))))
          (setq node parent-node
                range (evil-ts-obj--apply-modifiers parent-node thing spec))
        (setq range nil)))

    (when range
      (goto-char (1- (cadr range))))))



(defun evil-ts-obj--jump-boundaries ()
  (interactive)
  (when-let* ((thing evil-ts-obj-conf-nav-things)
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

(defun evil-ts-obj--next-thing (thing init-enclosing-node init-pos spec)
  "Return node and range that represents the next `THING'.
When `INIT-ENCLOSING-NODE' is not nil, it is considered to be a
node that represents current thing. Next thing should be
associated with the node that is not equal to
`INIT-ENCLOSING-NODE' and that starts after `INIT-POS'. Create
range according to provided `SPEC'."

  (let ((parent init-enclosing-node)
        (pos (treesit-node-end init-enclosing-node))
        next range)
    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move forward
        (setq next (treesit--thing-next init-pos thing)
              range (evil-ts-obj--apply-modifiers next thing spec))

      ;; try to move to next sibling from there and go to parent for next
      ;; iteration
      (while (and (or (null range)
                      ;; have to find a range that will move us to the right from
                      ;; the current position
                      (<= (car range) init-pos))
                  parent
                  (< pos (point-max)))
        (setq next (treesit--thing-next pos thing)
              range (evil-ts-obj--apply-modifiers next thing spec)
              parent (treesit-node-parent parent)
              pos (treesit-node-end parent))))

    (cons next range)))

(defun evil-ts-obj--goto-next-largest-thing (thing)
  "Go to the next `THING'.
At first, determine current THING at point. After that move to
the next largest `THING' that starts after the current thing
ends."
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         next
         range)

    (pcase-setq `(,next . ,range)
                (evil-ts-obj--next-thing thing init-enclosing-node init-pos spec))

    (when-let ((node next))
      (evil-ts-obj--maybe-set-jump init-enclosing-node node init-pos)
      (goto-char (car range)))))



(defun evil-ts-obj--goto-prev-largest-thing (thing)
  "Go to the previous `THING'.
At first, determine current THING at point. After that move to
the `THING' that ends before the current thing starts. If no such
a THING exists jump to a parent THING."
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing))
         (parent init-enclosing-node)
         (pos (treesit-node-start init-enclosing-node))
         prev range)

    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move backward
        (setq prev (treesit--thing-prev init-pos thing)
              range (evil-ts-obj--apply-modifiers prev thing spec))

      (while (and (or (null range)
                      (equal init-enclosing-node prev)
                      ;; have to find a range that will move us to the left from
                      ;; the current position
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


        (setq range (evil-ts-obj--apply-modifiers prev thing spec))))


    (when range
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--get-nav-thing (&optional current)
  "Return thing for the motion for current language.
Language is decided based on current point position.
If `CURRENT' is t, detect current thing at point and return this thing."
  (when-let* ((lang (treesit-language-at (point)))
              (thing (or (plist-get evil-ts-obj-conf-nav-things lang)
                         'compound)))
    ;; try to guess to what thing to move
    (when-let* (current
                ((listp thing))
                (node (evil-ts-obj--thing-around (point) thing))
                (cur-thing (evil-ts-obj--current-thing node thing)))
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

(defun evil-ts-obj--search-subtree-forward (start-node thing spec init-pos)
  (let ((filter (lambda (c) (< init-pos (treesit-node-end c))))
        (match-pred (lambda (n) (and (treesit-node-match-p n thing t)
                                     (let ((range (evil-ts-obj--apply-modifiers n thing spec)))
                                       (< init-pos (car range)))))))
    (evil-ts-obj--search-subtree start-node thing filter nil match-pred)))

(defun evil-ts-obj--search-subtree-backward (start-node thing spec init-pos)
  (let ((filter (lambda (c) (< (treesit-node-start c) init-pos)))
        (match-pred (lambda (n) (and (treesit-node-match-p n thing t)
                                     (let ((range (evil-ts-obj--apply-modifiers n thing spec)))
                                       (< (car range) init-pos)))))
        (node start-node)
        child)
    ;; have to find the deepest node for that subtree
    (while (setq node (evil-ts-obj--search-subtree node thing filter t match-pred))
      (setq child node))
    child))

(defun evil-ts-obj--goto-next-thing (thing)
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (init-enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         range)
    (if-let ((node init-enclosing-node)
             (next (evil-ts-obj--search-subtree-forward node thing spec init-pos)))
        (setq range (evil-ts-obj--apply-modifiers next thing spec))
      (pcase-setq `(,next . ,range)
                  (evil-ts-obj--next-thing thing init-enclosing-node init-pos spec)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--goto-prev-thing (thing)
  (let* ((spec (evil-ts-obj--make-nav-spec thing))
         (init-pos (point))
         (enclosing-node (evil-ts-obj--thing-around init-pos thing t))
         (cursor (or enclosing-node (evil-ts-obj--node-at-or-around init-pos)))
         range)

    ;; search backwardly inside current thing
    (if-let ((node enclosing-node)
             (prev (evil-ts-obj--search-subtree-backward node thing spec init-pos)))
        (setq range (evil-ts-obj--apply-modifiers prev thing spec))

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
              range (evil-ts-obj--apply-modifiers prev thing spec))))


    (when range
      (goto-char (car range)))))


;; * common predicates

(defun evil-ts-obj-common-param-pred (parent-regex node)
  "Predicate for detecting param thing.
Return t if `NODE' is named and its parent is matching against
`PARENT-REGEX'."
  (when-let* (((treesit-node-check node 'named))
              ((not (equal (treesit-node-type node) "comment")))
              (parent (treesit-node-parent node)))
    (string-match-p parent-regex (treesit-node-type parent))))


;; * modifiers

(defun evil-ts-obj--generic-find-sep-and-sibling (node node-kind-func next-node-func)
  "Return list (next-sibling next-sep next-term) for a `NODE'.
A function `NEXT-NODE-FUNC' should accept a `NODE' as an argument
and return next sibling node. `NODE-KIND-FUNC' is described in
`evil-ts-obj-generic-thing-with-sep-outer'."
  (let ((node-kind nil)
        next-sep next-sibling next-term)
    (when-let* ((next-node (funcall next-node-func node))
                (node-kind (funcall node-kind-func node node-kind next-node)))
      (pcase node-kind
        ('sep
         (setq next-sep next-node)
         (when-let* ((sibling (funcall next-node-func next-node))
                     (sibling-kind (funcall node-kind-func next-sep node-kind sibling)))

           (pcase sibling-kind
             ('sibling
              (setq next-sibling sibling))
             ('term
              (setq next-term sibling)))))
        ('sibling
         (setq next-sibling next-node))
        ('term
         (setq next-term next-node))))
    (list next-sibling next-sep next-term)))


(defun evil-ts-obj-generic-thing-with-sep-outer (node node-kind-func
                                                      node-fetcher
                                                      &optional extend-to-term)
  "Create a range for an outer text object that is associated with the `NODE'.
This function is mostly used to create outer text objects for
parameter or statement things. Things may be delimited by a
separator. `NODE-FETCHER' is a function that accepts two
arguments: direction (next or prev) and node, and returns sibling
of the passed node. See `evil-ts-obj--get-sibling-simple'.

`NODE-KIND-FUNC' is a function that accepts three arguments:
current node, current node kind and target node. It should
classify the passed target node and return its kind: sep,
sibling, term or nil. sep - means that passed node is a
recognized separator. sibling - denotes named node that is a
sibling of the current node. term - signals that this node is a
terminator of a sequence of things. When nil returned, act as if
current-node is the last node in the sequence. See
`evil-ts-obj--get-node-kind' or
`evil-ts-obj--get-node-kind-strict'.

This function works as follows: at first, it tries to find the
next sibling node. If such a node exists, start of this node
defines the outer range end. If there is no next sibling node,
but `EXTEND-TO-TERM' is t and next node has kind term, then
start of term node becomes new end of the outer range. If there
is no next sibling, outer range is extended to the end of the
previous sibling. It does so only if no trailing separators were
found."
  (pcase-let* ((start-pos (treesit-node-start node))
               (end-pos (treesit-node-end node))
               (`(,next-sibling ,next-sep ,next-term)
                (evil-ts-obj--generic-find-sep-and-sibling node node-kind-func
                                                           (apply-partially node-fetcher 'next))))

    (cond (next-sibling
           (setq end-pos (treesit-node-start next-sibling)))
          ((and extend-to-term next-term)
           (setq end-pos (treesit-node-start next-term)))
          (next-sep
           (setq end-pos (treesit-node-end next-sep))))

    (unless next-sibling
      ;; this is the last thing in a sequence
      ;; determine how to extend to the previous separator
      (pcase-let ((`(,prev-sibling ,prev-sep ,prev-term)
                   (evil-ts-obj--generic-find-sep-and-sibling node node-kind-func
                                                              (apply-partially node-fetcher 'prev))))
        (cond ((and (null next-sep) prev-sibling)
               ;; There was not trailing separator.
               ;; So we can assume that the previous one should be included.
               (setq start-pos (treesit-node-end prev-sibling)))
              ((and extend-to-term prev-term)
               (setq start-pos (treesit-node-end prev-term)))
              ((and next-sep prev-sep)
               ;; There was trailing separator.
               ;; Extend to the end of the previous separator.
               (setq start-pos (treesit-node-end prev-sep))))))
    (list start-pos end-pos)))

(defun evil-ts-obj--get-sibling-simple (dir node)
  "Implementation of a node-fetcher for `evil-ts-obj-generic-thing-with-sep-outer'.
Return a next or previous sibling for `NODE' based on value of
`DIR'."
  (pcase dir
    ('next (treesit-node-next-sibling node))
    ('prev (treesit-node-prev-sibling node))))

(defun evil-ts-obj--get-sibling-bin-op (node-types dir node)
  "Traverse typical tree structure of binary operators.
Return sibling of a given `NODE' in the specified direction
`DIR'. Parent of the `NODE' should have one of the type from the
`NODE-TYPES'. For a tree like below, when `NODE' is (3) and `DIR'
is prev, jump to the the last child of its sibling (node (5)).
When node is (5) and `DIR' is next jump to its grandparent's
second child (node (3)).

         (1) bin_op----------
               |            |
         (2) bin_op------  (3) and i < 9
               |        |
         (4) bin_op    (5) and j < 3
               |
    (6) i > 0 and j > 0"

  (when-let* ((parent (treesit-node-parent node))
              ((member (treesit-node-type parent) node-types)))
    (let ((sibling (evil-ts-obj--get-sibling-simple dir node)))
      (pcase (treesit-node-type sibling)
        ('()
         (when-let* (((eq dir 'next))
                     (grandparent (treesit-node-parent parent))
                     ((member (treesit-node-type grandparent) node-types)))
           (treesit-node-child grandparent 1)))
        ((pred (seq-contains-p node-types))
         (when (eq dir 'prev)
           (treesit-node-child sibling -1)))
        (_ sibling)))))

(defun evil-ts-obj--nodes-on-the-same-line (node other-node)
  "Return t if `NODE' and `OTHER-NODE' end on the same line."
  (save-excursion
    (goto-char (treesit-node-end node))
    (let ((other-end (treesit-node-end other-node)))
      (and
       (<= other-end (line-end-position))
       (>= other-end (line-beginning-position))))))

(defun evil-ts-obj--get-node-kind-strict (sep-regex cur-node cur-kind node)
  "Return sibling kind for `NODE' only if it is preceded by the separator.
Implementation of a node-kind-func for
`evil-ts-obj-generic-thing-with-sep-outer'.

Return sep if `NODE' matches against `SEP-REGEX'. Otherwise
return sibling if `NODE' is named and current node is
separator (`CUR-KIND'). Return term is `NODE' is anonymous and on
the same line as `CUR-NODE' (examples of possible termination nodes:
\) ,\] ,end ,fi, etc.)."

  (if (string-match-p sep-regex (treesit-node-type node))
      'sep

    (let ((named (treesit-node-check node 'named)))
      (pcase (cons named cur-kind)
        (`(t . ,'sep) 'sibling)
        ((and `(nil . ,_)
              (guard (evil-ts-obj--nodes-on-the-same-line node cur-node)))
         'term)))))

(defun evil-ts-obj--get-node-kind (sep-regex cur-node _cur-kind node)
  "Return sibling kind for `NODE' if it is named.
Implementation of a node-kind-func for
`evil-ts-obj-generic-thing-with-sep-outer'.

Return sep if `NODE' matches against `SEP-REGEX'. Otherwise
return sibling if `NODE' is named. Return term is `NODE' is
anonymous."

  (cond
   ((and sep-regex
         (string-match-p sep-regex (treesit-node-type node)))
    'sep)
   ((treesit-node-check node 'named)
    'sibling)
   ((when (evil-ts-obj--nodes-on-the-same-line node cur-node)
      'term))))

(defun evil-ts-obj-thing-with-sep-outer (node sep-regex &optional extend-to-term strict)
  "Create a range for an outer text object that is associated with the `NODE'.
It uses `evil-ts-obj-generic-thing-with-sep-outer'
underneath (examine this function for an explanation of
`EXTEND-TO-TERM'). If `STRICT' is t use
`evil-ts-obj--get-node-kind-strict' as node-kind-func, otherwise
use `evil-ts-obj--get-node-kind'. See those function description
for `SEP-REGEX' meaning."

  (evil-ts-obj-generic-thing-with-sep-outer
   node
   (apply-partially
    (if strict
        #'evil-ts-obj--get-node-kind-strict
      #'evil-ts-obj--get-node-kind)
    sep-regex)
   #'evil-ts-obj--get-sibling-simple
   extend-to-term))


(defun evil-ts-obj-param-outer-mod (node sep-regex)
  "Create parameter outer text object.
This function invokes `evil-ts-obj-thing-with-sep-outer' with
extend-to-term and strict set to t. See
`evil-ts-obj-thing-with-sep-outer' for information about `NODE'
and `SEP-REGEX'."
  (evil-ts-obj-thing-with-sep-outer node sep-regex t t))

(defun evil-ts-obj-param-outer-universal-mod (node &optional sep-regex)
  "Create parameter outer text object.
This function invokes `evil-ts-obj-thing-with-sep-outer' with
extend-to-term set to t. See `evil-ts-obj-thing-with-sep-outer'
for information about `NODE' and `SEP-REGEX'."
  (evil-ts-obj-thing-with-sep-outer node sep-regex t nil))

(defun evil-ts-obj-generic-thing-upper (node node-kind-func node-fetcher &optional extend-to-next)
  "Create a range for an upper text object that is associated with `NODE'.
Description of `NODE-FETCHER' and `NODE-KIND-FUNC' see in
`evil-ts-obj-generic-thing-with-sep-outer'.

This function works as follows: at first, it fetches the next
sibling node. If there is separator between current and the
sibling node or `EXTEND-TO-NEXT' is t, then start of sibling node
becomes new end of the upper range. If there is no next sibling
node, and next node has kind term, then start of the term node
becomes new end of the upper range. Then it iterates over all
nodes returned by the `NODE-FETCHER' in the prev direction. It
stops on the first node, which kind is not either sibling or
sep (kind is obtained from `NODE-KIND-FUNC'). The range start is
a start of the last sibling node returned by the `NODE-FETCHER'."
  (pcase-let* ((start-pos (treesit-node-start node))
               (end-pos (treesit-node-end node))
               (`(,next-sibling ,next-sep ,next-term)
                (evil-ts-obj--generic-find-sep-and-sibling node node-kind-func
                                                           (apply-partially node-fetcher 'next))))

    (when (or extend-to-next
              next-sep)
      (if next-sibling
          (setq end-pos (treesit-node-start next-sibling))
        (when next-term
          (setq end-pos (treesit-node-start next-term)))))

    (let ((final-sibling node)
          prev
          cur-kind)
      (while (and (setq prev (funcall node-fetcher 'prev node))
                  (setq cur-kind (funcall node-kind-func node cur-kind prev))
                  (setq node prev)
                  (memq cur-kind '(sibling sep)))
        (setq final-sibling node))
      (setq start-pos (treesit-node-start final-sibling)))

    (list start-pos end-pos)))

(defun evil-ts-obj-param-upper-mod (node sep-regex)
"Create parameter upper text object.
This function invokes `evil-ts-obj-generic-thing-upper' with
extend-to-next set to t. See `evil-ts-obj--get-node-kind' for
information about `NODE' and `SEP-REGEX'."
 (evil-ts-obj-generic-thing-upper
   node
   (apply-partially #'evil-ts-obj--get-node-kind sep-regex)
   #'evil-ts-obj--get-sibling-simple
   t))

(defun evil-ts-obj-generic-thing-lower (node node-kind-func node-fetcher &optional extend-to-prev)
  "Create a range for an lower text object that is associated with `NODE'.
Description of `NODE-FETCHER' and `NODE-KIND-FUNC' see in
`evil-ts-obj-generic-thing-with-sep-outer'.

This function works as follows: at first, it fetches the previous
sibling node. If there is separator between current and the
sibling node or `EXTEND-TO-PREV' is t, then end of sibling node
becomes new start of the lower range. If there is no previous
sibling node, and previous node has kind term, then end of the
term node becomes new start of the lower range. Then it iterates
over all nodes returned by the `NODE-FETCHER' in the next
direction. It stops on the first node, which kind is not either
sibling or sep (the kind is obtained from `NODE-KIND-FUNC'). The
range end is an end of the last sibling node returned by the
`NODE-FETCHER'."
  (pcase-let* ((start-pos (treesit-node-start node))
               (end-pos (treesit-node-end node))
               (`(,prev-sibling ,prev-sep ,prev-term)
                (evil-ts-obj--generic-find-sep-and-sibling node node-kind-func
                                                           (apply-partially node-fetcher 'prev))))

    (when (or extend-to-prev
              prev-sep)
      (if prev-sibling
          (setq start-pos (treesit-node-end prev-sibling))
        (when prev-term
          (setq start-pos (treesit-node-end prev-term)))))

    (let ((final-sibling node)
          next
          cur-kind)
      (while (and (setq next (funcall node-fetcher 'next node))
                  (setq cur-kind (funcall node-kind-func node cur-kind next))
                  (setq node next)
                  (memq cur-kind '(sibling sep)))
        (setq final-sibling node))
      (setq end-pos (treesit-node-end final-sibling)))

    (list start-pos end-pos)))

(defun evil-ts-obj-param-lower-mod (node sep-regex)
  "Create parameter lower text object.
This function invokes `evil-ts-obj-generic-thing-lower' with
extend-to-prev set to t. See `evil-ts-obj--get-node-kind' for
information about `NODE' and `SEP-REGEX'."
  (evil-ts-obj-generic-thing-lower
   node
   (apply-partially #'evil-ts-obj--get-node-kind sep-regex)
   #'evil-ts-obj--get-sibling-simple
   t))


;; * extentision functions
(defun evil-ts-obj-common-statement-ext (spec node sep-regex &optional get-sibling-func)
  "Common statetement extension function."
  (pcase spec
    ((pmap (:mod 'outer))
     (evil-ts-obj-generic-thing-with-sep-outer
      node
      (apply-partially #'evil-ts-obj--get-node-kind-strict
                       sep-regex)
      (or get-sibling-func #'evil-ts-obj--get-sibling-simple)))

    ((pmap (:mod 'upper))
     (evil-ts-obj-generic-thing-upper
      node
      (apply-partially #'evil-ts-obj--get-node-kind sep-regex)
      (or get-sibling-func #'evil-ts-obj--get-sibling-simple)))

    ((pmap (:mod 'lower))
     (evil-ts-obj-generic-thing-lower
      node
      (apply-partially #'evil-ts-obj--get-node-kind
                         sep-regex)
      (or get-sibling-func #'evil-ts-obj--get-sibling-simple)))))

(defun evil-ts-obj-common-param-ext (spec node &optional sep-regex universal)
  (pcase spec
    ((pmap (:mod 'outer))
     (if (or universal
             (null sep-regex))
         (evil-ts-obj-param-outer-universal-mod node sep-regex)
       (evil-ts-obj-param-outer-mod node sep-regex)))
    ((pmap (:mod 'upper))
     (evil-ts-obj-param-upper-mod node sep-regex))
    ((pmap (:mod 'lower))
     (evil-ts-obj-param-lower-mod node sep-regex))))

(provide 'evil-ts-obj-core)
;;; evil-ts-obj-core.el ends here
