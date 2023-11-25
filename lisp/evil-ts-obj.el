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

(defun evil-ts-obj--root-at (pos)
  (or (when-let ((parser
                  (car (treesit-local-parsers-at pos))))
        (treesit-parser-root-node parser))
      (treesit-buffer-root-node (treesit-language-at pos))))

(defun evil-ts-obj--smallest-node-at (pos &optional named)
  (let* ((root (evil-ts-obj--root-at pos))
         (node  (treesit-node-on pos pos nil named)))
    (when (not (treesit-node-eq node root))
      node)))

(defun evil-ts-obj--thing-around (pos thing &optional dont-step-forward)
  "Return the enclosing thing outer POS.

THING should be a thing defined in `treesit-thing-settings',
which see; it can also be a predicate."
  (if-let* ((cursor (treesit-node-at pos))
            (iter-pred (lambda (node)
                         (and (<= (treesit-node-start node)
                                  pos
                                  (treesit-node-end node))
                              (treesit-node-match-p node thing t))))
            (enclosing-node (treesit-parent-until cursor iter-pred t)))
      enclosing-node
    ;; try to find next node
    (unless dont-step-forward
      (treesit--thing-next (point) thing))))

(defun evil-ts-obj--current-thing (node nav-thing)
  (pcase nav-thing
    ((or (pred symbolp)
         (pred stringp))
     nav-thing)
    ((pred listp)
     (seq-find (lambda (thing) (treesit-node-match-p node thing t))
               (cdr nav-thing) nil))
    (_ (error "Unsupported thing %s" nav-thing))))

(defun evil-ts-obj--check-modifier-scope (modifier-plist scope)
  (when-let ((scopes (plist-get modifier-plist :scope)))
    (pcase scopes
      ((pred symbolp)
       (eq scope scopes))
      ((pred listp)
       (memq scope scopes))
      (_ nil))))

(defun evil-ts-obj--find-mod-func (modifiers-list scope)
  (when-let ((mod-plist (seq-find (lambda (mod-plist)
                                    (evil-ts-obj--check-modifier-scope mod-plist scope))
                                  modifiers-list)))
    (plist-get mod-plist :func)))

(defun evil-ts-obj--apply-modifiers (node pos thing scope)
  (when node
    (if-let* ((modifiers-plist (plist-get evil-ts-obj-conf-thing-modifiers
                                          (treesit-language-at pos)))
              (current-thing (evil-ts-obj--current-thing node thing))
              (thing-modifiers (plist-get modifiers-plist current-thing))
              (mod-func (evil-ts-obj--find-mod-func thing-modifiers scope))
              (range (funcall mod-func node)))
        range
      (list (treesit-node-start node)
            (treesit-node-end node)))))


(defun evil-ts-obj--get-thing-range (pos thing scope &optional return-node)
  (when-let ((node (evil-ts-obj--thing-around pos thing)))
    (append
     (evil-ts-obj--apply-modifiers node pos thing scope)
     (when return-node (list node)))))


(defun evil-ts-obj--begin-of-thing (thing)
  (when-let* ((range (evil-ts-obj--get-thing-range (point) thing 'nav t))
              (node (car (last range))))
    (while (and range
                (<= (point) (car range)))
      ;; jump to beginning of a parent.
      ;; Take into consideration that parent may start on the same position as the child.
      (if-let ((parent-node (treesit-parent-until
                             node (lambda (n) (treesit-node-match-p n thing t)))))
          (progn
            (setq node parent-node
                  range (evil-ts-obj--apply-modifiers
                         parent-node (treesit-node-start parent-node) thing 'nav))
            (evil-set-jump))
        (setq range nil)))
    (when range
      (goto-char (car range)))))

(defun evil-ts-obj--end-of-thing (thing)
  (when-let ((range (evil-ts-obj--get-thing-range (point) thing 'nav t))
             (node (car (last range))))
    (while (and range
                (<= (1- (cadr range)) (point)))
      (if-let (parent-node (treesit-parent-until
                            node (lambda (n) (treesit-node-match-p n thing t))))
          (progn
            (setq node parent-node
                  range (evil-ts-obj--apply-modifiers
                         parent-node (treesit-node-end parent-node) thing 'nav))
            (evil-set-jump))
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

(defun evil-ts-obj--next-sibling (thing)
  (let* ((init-pos (point))
         (init-enclosing-node (treesit--thing-at init-pos thing))
         next)

    ;; simple case: next sibling exists
    (setq next (evil-ts-obj--find-matching-sibling thing init-enclosing-node))
    (unless next
      (setq next (evil-ts-obj--next-thing thing init-enclosing-node init-pos)))

    (when-let ((node next)
               (range (evil-ts-obj--apply-modifiers
                       node (treesit-node-start node) thing 'nav)))
      (evil-ts-obj--maybe-set-jump init-enclosing-node node init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--prev-thing (thing init-enclosing-node init-pos)
  (let ((pos (treesit-node-start init-enclosing-node))
        prev range)
    (if (null init-enclosing-node)
        ;; it seems we are outside of any thing at the top level
        ;; just try to move backward
        (setq prev (treesit--thing-prev init-pos thing)
              range (evil-ts-obj--apply-modifiers prev pos thing 'nav))

      (while (and (or (null range)
                      (and init-enclosing-node
                           (equal init-enclosing-node prev))
                      ;; have to range that will move (<-) us from the current point
                      (<= init-pos (car range)))
                  (< (point-min) pos))
        ;; trying to get prev
        (setq prev (treesit--thing-prev pos thing))

        (when (null prev)
          ;; if no previous sibling
          ;; try to step up till we move from the start position of current enclosing node
          ;; make one step per iteration
          (setq prev (treesit--thing-at pos thing t)
                pos (or (treesit-node-start prev) 0)))
        (setq range (evil-ts-obj--apply-modifiers prev pos thing 'nav))))
    (cons prev range)))

(defun evil-ts-obj--prev-sibling (thing)
  (let* ((init-pos (point))
         (init-enclosing-node (treesit--thing-at init-pos thing))
         prev range)

    ;; simple case: prev sibling exists
    (setq prev (evil-ts-obj--find-matching-sibling thing init-enclosing-node t)
          range (evil-ts-obj--apply-modifiers prev (treesit-node-start prev) thing 'nav))
    (unless prev
      (pcase-let ((`(,p . ,r) (evil-ts-obj--prev-thing thing init-enclosing-node init-pos)))
        (setq prev p
              range r)))

    (when range
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--thing-sibling (dir)
  (let ((thing (or evil-ts-obj-conf-nav-thing
                   'compound)))
    (if (> dir 0)
        (evil-ts-obj--next-sibling thing)
      (evil-ts-obj--prev-sibling thing))))

(defun evil-ts-obj--current-thing-sibling (dir)
  (let ((thing (or evil-ts-obj-conf-nav-thing
                   'compound)))

    ;; try to guess to what thing to move
    (when-let* ((nav-thing evil-ts-obj-conf-nav-thing)
                (node (evil-ts-obj--thing-around (point) nav-thing))
                (cur-thing (evil-ts-obj--current-thing node nav-thing)))
      ;; find next sibling of the thing we are currently at
      (setq thing cur-thing))

    (if (> dir 0)
        (evil-ts-obj--next-sibling thing)
      (evil-ts-obj--prev-sibling thing))))

(defun evil-ts-obj--search-subtree (node thing &optional backward child-filter)
  ;; cant use treesit-search-subtree since it includes root node to the search
  (let* ((all-children (treesit-node-children node t))
         (children (if child-filter
                       (funcall child-filter all-children)
                     all-children))
         result)
    (cl-loop for child in (if backward
                              (nreverse children)
                            children)
             do (setq result (treesit-search-subtree
                              child
                              (lambda (n) (treesit-node-match-p n thing t))
                              backward))
             when result
             return result)))

(defun evil-ts-obj--search-subtree-backward (start-node thing init-pos)
  ;; have to find the deepest node for that subtree
  (let ((filter (apply-partially #'seq-filter
                                 (lambda (c) (< (treesit-node-end c) init-pos))))
        (node start-node)
        child)
    (while (setq node (evil-ts-obj--search-subtree node thing t filter))
      (setq child node))
    child))

(defun evil-ts-obj--find-next-thing (thing pos)
  (let ((enclosing-node (evil-ts-obj--smallest-node-at pos)))
    (if-let ((node enclosing-node)
             (next (evil-ts-obj--search-subtree node thing)))
        next
      (evil-ts-obj--next-thing thing enclosing-node pos))))

(defun evil-ts-obj--goto-next-thing (thing)
  (when-let* ((init-pos (point))
              (next (evil-ts-obj--find-next-thing thing init-pos))
              (range (evil-ts-obj--apply-modifiers
                      next (treesit-node-start next) thing 'nav)))
    (goto-char (car range))))

(defun evil-ts-obj--goto-prev-thing (thing)
  (pcase-let* ((init-pos (point))
               (enclosing-node (evil-ts-obj--smallest-node-at init-pos))
               (`(,prev . ,range) (evil-ts-obj--prev-thing thing enclosing-node init-pos)))


    (when-let ((node prev)
               (child (evil-ts-obj--search-subtree-backward node thing init-pos)))
      (setq prev child
            range (evil-ts-obj--apply-modifiers
                   prev (treesit-node-start prev) thing 'nav)))

    (when range
      (goto-char (car range)))))



;; * interactive functions

;;;###autoload
(defun evil-ts-obj-next-sibling-thing ()
  (interactive)
  (evil-ts-obj--thing-sibling 1))

;;;###autoload
(defun evil-ts-obj-same-next-sibling-thing ()
  (interactive)
  (evil-ts-obj--current-thing-sibling 1))

;;;###autoload
(defun evil-ts-obj-prev-sibling-thing ()
  (interactive)
  (evil-ts-obj--thing-sibling -1))

;;;###autoload
(defun evil-ts-obj-same-prev-sibling-thing ()
  (interactive)
  (evil-ts-obj--current-thing-sibling -1))


;;;###autoload
(defun evil-ts-obj-begin-of-thing ()
  (interactive)
  (if current-prefix-arg
      (evil-ts-obj--begin-of-thing 'compound)
    (evil-ts-obj--begin-of-thing evil-ts-obj-conf-nav-thing)))

;;;###autoload
(defun evil-ts-obj-end-of-thing ()
  (interactive)
  (if current-prefix-arg
      (evil-ts-obj--end-of-thing 'compound)
    (evil-ts-obj--end-of-thing evil-ts-obj-conf-nav-thing)))



;;;###autoload
(defun evil-ts-obj-next-thing ()
  (interactive)
  (let ((thing (or evil-ts-obj-conf-nav-thing
                   'compound)))
    (evil-ts-obj--goto-next-thing thing)))

;;;###autoload
(defun evil-ts-obj-prev-thing ()
  (interactive)
  (let ((thing (or evil-ts-obj-conf-nav-thing
                   'compound)))
    (evil-ts-obj--goto-prev-thing thing)))


(evil-define-text-object evil-ts-obj-param-outer (count &optional _beg _end _type)
  "Select a param object."
  (evil-ts-obj--get-thing-range (point) 'param 'outer))

(evil-define-text-object evil-ts-obj-param-inner (count &optional _beg _end _type)
  "Select a param object."
  (evil-ts-obj--get-thing-range (point) 'param 'inner))

(defun evil-ts-obj--finalize-compound (range)
  (pcase-let ((`(,first-pos ,last-pos) range))
    (when (eq (char-before last-pos) ?\n)
      (setq last-pos (1- last-pos)))
    (list first-pos last-pos)))

(evil-define-text-object evil-ts-obj-compound-outer (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--finalize-compound
   (evil-ts-obj--get-thing-range (point) 'compound 'outer)))


(evil-define-text-object evil-ts-obj-compound-inner (count &optional _beg _end _type)
  "Select a compound inner object."
  (evil-ts-obj--get-thing-range (point) 'compound 'inner))

(evil-define-text-object evil-ts-obj-statement-outer (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--get-thing-range (point) 'statement 'outer))

(evil-define-text-object evil-ts-obj-statement-inner (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--get-thing-range (point) 'statement 'inner))

(defun evil-ts-obj--get-thing-upper-range (pos thing scope)

  (pcase-let* ((`(,start ,end ,node) (evil-ts-obj--get-thing-range pos thing scope t))
               (final-sibling node))
    (while (setq node (treesit-node-prev-sibling node t))
      (setq final-sibling node))
    (setq start (treesit-node-start final-sibling))
    (list start end)))

(evil-define-text-object evil-ts-obj-compound-upper (count &optional _beg _end _type)
  "Select a compound thing and its previos siblings."
  (evil-ts-obj--get-thing-upper-range (point) 'compound 'outer))

(evil-define-text-object evil-ts-obj-statement-upper (count &optional _beg _end _type)
  "Select a statement thing and its previos siblings."
  (evil-ts-obj--get-thing-upper-range (point) 'statement 'outer))

(evil-define-text-object evil-ts-obj-param-upper (count &optional _beg _end _type)
  "Select a param thing and its previos siblings."
  (evil-ts-obj--get-thing-upper-range (point) 'param 'outer))

(defun evil-ts-obj--get-thing-lower-range (pos thing scope)

  (pcase-let* ((`(,start ,end ,node) (evil-ts-obj--get-thing-range pos thing scope t))
               (final-sibling node))
    (while (setq node (treesit-node-next-sibling node t))
      (setq final-sibling node))
    (setq end (treesit-node-end final-sibling))
    (list start end)))

(evil-define-text-object evil-ts-obj-compound-lower (count &optional _beg _end _type)
  "Select a compound thing and its next siblings."
  (evil-ts-obj--get-thing-lower-range (point) 'compound 'outer))

(evil-define-text-object evil-ts-obj-statement-lower (count &optional _beg _end _type)
  "Select a statement thing and its previos siblings."
  (evil-ts-obj--get-thing-lower-range (point) 'statement 'outer))

(evil-define-text-object evil-ts-obj-param-lower (count &optional _beg _end _type)
  "Select a param thing and its previos siblings."
  (evil-ts-obj--get-thing-lower-range (point) 'param 'outer))


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
  (kbd "M-a") #'evil-ts-obj-begin-of-thing
  (kbd "M-e") #'evil-ts-obj-end-of-thing
  (kbd "M-n") #'evil-ts-obj-next-sibling-thing
  (kbd "C-M-n") #'evil-ts-obj-same-next-sibling-thing
  (kbd "M-p") #'evil-ts-obj-prev-sibling-thing
  (kbd "C-M-p") #'evil-ts-obj-same-prev-sibling-thing
  (kbd "M-f") #'evil-ts-obj-next-thing
  (kbd "M-b") #'evil-ts-obj-prev-thing)



(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
