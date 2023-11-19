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

(require 'avy)
(require 'treesit)
(require 'evil)

(require 'evil-ts-obj-conf)

(defun evil-ts-obj--thing-around (pos thing)
  "Return the enclosing thing around POS.

THING should be a thing defined in `treesit-thing-settings',
which see; it can also be a predicate."
  (let ((cursor (treesit-node-at pos))
        (iter-pred (lambda (node)
                     (and (<= (treesit-node-start node)
                              pos
                              (treesit-node-end node))
                          (treesit-node-match-p node thing t)))))
    (treesit-parent-until cursor iter-pred t)))

(defun evil-ts-obj--current-thing (node dwim-thing)
  (pcase dwim-thing
    ((or (pred symbolp)
         (pred stringp))
     dwim-thing)
    ((pred listp)
     (seq-find (lambda (thing) (treesit-node-match-p node thing t))
               (cdr dwim-thing) nil))
    (_ (error "Unsupported dwim thing %s" dwim-thing))))

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


(defun evil-ts-obj-begin-of-thing (thing)
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

(defun evil-ts-obj-end-of-thing (thing)
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
  (when-let* ((thing evil-ts-obj-conf-nav-dwim-thing)
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

(defun evil-ts-obj--next-sibling (thing)

  (let* ((init-pos (point))
         (init-enclosing-node (treesit--thing-at init-pos thing))
         (next (evil-ts-obj--find-matching-sibling thing init-enclosing-node))
         (parent init-enclosing-node)
         (pos init-pos))

    (if (and (null next)
             (null init-enclosing-node))
        ;; it seems we are outside of any thing at the top level
        ;; just try to move forward
        (setq next (treesit--thing-next init-pos thing))

      ;; go to parent and try to move to next sibling from there
      (while (and (or (null next)
                      (= init-pos (treesit-node-start next)))
                  (setq parent (treesit-node-parent parent))
                  (< pos (point-max)))
        (setq pos (treesit-node-end parent)
              next (treesit--thing-next pos thing))))

    (when-let ((node next)
               (range (evil-ts-obj--apply-modifiers
                       node (treesit-node-start node) thing 'nav)))
      (evil-ts-obj--maybe-set-jump init-enclosing-node node init-pos)
      (goto-char (car range)))))

(defun evil-ts-obj--prev-sibling (thing)
  (let* ((init-pos (point))
         (init-enclosing-node (treesit--thing-at init-pos thing))
         (pos (treesit-node-start init-enclosing-node))
         (prev (evil-ts-obj--find-matching-sibling thing init-enclosing-node t))
         (range (evil-ts-obj--apply-modifiers prev pos thing 'nav)))
    (if (and (null prev)
             (null init-enclosing-node))
        ;; it seems we are outside of any thing at the top level
        ;; just try to move bacward
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

    (when range
      (evil-ts-obj--maybe-set-jump init-enclosing-node prev init-pos)
      (goto-char (car range)))))



(defun evil-ts-obj--sibling-dwim (dir)
  (let ((thing (or evil-ts-obj-conf-nav-dwim-thing
                   'compound)))

    ;; try to guess to what thing to move
    (when-let* ((dwim-thing evil-ts-obj-conf-nav-dwim-thing)
                (node (evil-ts-obj--thing-around (point) dwim-thing))
                (cur-thing (evil-ts-obj--current-thing node dwim-thing)))
      ;; find next sibling of the thing we are currently at
      (setq thing cur-thing))

    (if (> dir 0)
        (evil-ts-obj--next-sibling thing)
      (evil-ts-obj--prev-sibling thing))))

(defun evil-ts-obj--search-subtree (node thing &optional backward)
  ;; cant use treesit-search-subtree since it includes root node to the search
  (let ((children (treesit-node-children node t))
        result)
    (cl-loop for child in (if backward
                              (nreverse children)
                            children)
             do  (setq result (treesit-search-subtree
                               child
                               (lambda (n) (treesit-node-match-p n thing t))
                               backward))
             when result
             return result)))



(defun evil-ts-obj--forward (thing)
  (when-let ((enclosing-node (treesit-node-at (point))))
    (if-let ((child-node (evil-ts-obj--search-subtree enclosing-node thing)))
        ;; found node in the subtree
        (goto-char (treesit-node-start child-node))
      (evil-ts-obj--move-to-sibling (treesit-node-end enclosing-node) 1 thing))))

(defun evil-ts-obj--backward (thing)
  (evil-ts-obj--move-to-sibling (point) -1 thing)
  (message "prev node %s" (evil-ts-obj--thing-around (point) thing))
  (when-let* ((prev-node (evil-ts-obj--thing-around (point) thing))
              (child-node (evil-ts-obj--search-subtree prev-node thing t)))
    ;; found node in the subtree
    (goto-char (treesit-node-start child-node)))
  ;; (when-let ((enclosing-node (treesit-node-at (point))))
  ;;   (if-let ((child-node (evil-ts-obj--search-subtree enclosing-node thing t)))
  ;;       ;; found node in the subtree
  ;;       (progn
  ;;         (message "found child %s" child-node)
  ;;         (goto-char (treesit-node-end child-node)))
  ;;     (evil-ts-obj--move-to-sibling (treesit-node-start enclosing-node) -1 thing)))
  )



;; * dwim functions

;;;###autoload
(defun evil-ts-obj-next-sibling-dwim ()
  (interactive)
  (evil-ts-obj--sibling-dwim 1))

;;;###autoload
(defun evil-ts-obj-prev-sibling-dwim ()
  (interactive)
  (evil-ts-obj--sibling-dwim -1))


;;;###autoload
(defun evil-ts-obj-begin-of-thing-dwim ()
  (interactive)
  (if current-prefix-arg
      (evil-ts-obj-begin-of-thing 'compound)
    (evil-ts-obj-begin-of-thing evil-ts-obj-conf-nav-dwim-thing)))



;;;###autoload
(defun evil-ts-obj-end-of-thing-dwim ()
  (interactive)
  (if current-prefix-arg
      (evil-ts-obj-end-of-thing 'compound)
    (evil-ts-obj-end-of-thing evil-ts-obj-conf-nav-dwim-thing)))



;;;###autoload
(defun evil-ts-obj-forward-dwim ()
  (interactive)
  (if-let* ((dwim-thing evil-ts-obj-conf-nav-dwim-thing))
      (evil-ts-obj--forward dwim-thing)))

;;;###autoload
(defun evil-ts-obj-backward-dwim ()
  (interactive)
  (if-let* ((dwim-thing evil-ts-obj-conf-nav-dwim-thing))
      (evil-ts-obj--backward dwim-thing)))


(evil-define-text-object evil-ts-obj-param-around (count &optional _beg _end _type)
  "Select a param object."
  (evil-ts-obj--get-thing-range (point) 'param 'around))

(evil-define-text-object evil-ts-obj-param-inner (count &optional _beg _end _type)
  "Select a param object."
  (evil-ts-obj--get-thing-range (point) 'param 'inner))

(defun evil-ts-obj--finalize-compound (range)
  (pcase-let ((`(,first-pos ,last-pos) range))
    (when (eq (char-before last-pos) ?\n)
      (setq last-pos (1- last-pos)))
    (list first-pos last-pos)))

(evil-define-text-object evil-ts-obj-compound-around (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--finalize-compound
   (evil-ts-obj--get-thing-range (point) 'compound 'around)))


(evil-define-text-object evil-ts-obj-compound-inner (count &optional _beg _end _type)
  "Select a compound inner object."
  (evil-ts-obj--get-thing-range (point) 'compound 'inner))

(evil-define-text-object evil-ts-obj-statement-around (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--get-thing-range (point) 'statement 'around))

(evil-define-text-object evil-ts-obj-statement-inner (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--get-thing-range (point) 'statement 'inner))


;;;###autoload
(defun evil-ts-obj-avy-jump (&optional action)
  (interactive)
  (setq avy-action (or action #'avy-action-goto))
  (when-let ((query evil-ts-obj-conf-avy-jump-query)
             (root (treesit-buffer-root-node)))

    (avy-process
     (mapcar
      (lambda (c) (cons c (selected-window)))
      ;; treesit returns objects that are not visible but are children of a visible top-level thing
      ;; filter them
      (seq-filter (lambda (pair) (and (>= (car pair) (window-start))))
                  (treesit-query-range root query (window-start) (window-end)))))))


;;* default keybindings and minor mode

(defvar evil-ts-obj-inner-text-objects-map
  (let ((map (make-sparse-keymap "Inner text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-inner)
    (define-key map (kbd "a") #'evil-ts-obj-param-inner)
    (define-key map (kbd "s") #'evil-ts-obj-statement-inner)
    map))
(defvar evil-ts-obj-around-text-objects-map
  (let ((map (make-sparse-keymap "Around text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-around)
    (define-key map (kbd "a") #'evil-ts-obj-param-around)
    (define-key map (kbd "s") #'evil-ts-obj-statement-around)
    map))



;;;###autoload
(define-minor-mode evil-ts-obj-mode
  "A minor mode with tree sitter keybinds."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))


(evil-define-key '(visual operator) evil-ts-obj-mode-map
  "i" evil-ts-obj-inner-text-objects-map
  "a" evil-ts-obj-around-text-objects-map)

(evil-define-key 'normal evil-ts-obj-mode-map
  (kbd "M-a") #'evil-ts-obj-begin-of-thing-dwim
  (kbd "M-e") #'evil-ts-obj-end-of-thing-dwim
  (kbd "M-n") #'evil-ts-obj-next-sibling-dwim
  (kbd "M-p") #'evil-ts-obj-prev-sibling-dwim
  (kbd "M-f") #'evil-ts-obj-forward-dwim
  (kbd "M-b") #'evil-ts-obj-backward-dwim
  "za" #'evil-ts-obj-avy-jump)



(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
