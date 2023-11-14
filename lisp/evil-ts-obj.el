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

(defun evil-ts-obj-begin-of-thing (thing)
  (when-let ((node (evil-ts-obj--thing-around (point) thing)))
    (while (and node
                (= (treesit-node-start node) (point)))
      ;; jump to beginning of a parent.
      ;; Take into consideration that parent may start on the same position as the child.
      (if-let ((parent-thing (treesit-parent-until
                              node (lambda (n) (treesit-node-match-p n thing t)))))
          (progn
            (setq node parent-thing)
            (evil-set-jump))
        (setq node nil)))
    (when node
      (goto-char (treesit-node-start node)))))

(defun evil-ts-obj-end-of-thing (thing)
  (when-let ((node (evil-ts-obj--thing-around (point) thing)))
    ;; point can be on the next whitespace after the end
    (while (and node
                (>= (point) (1- (treesit-node-end node))))
      (if-let (parent-thing (treesit-parent-until
                             node (lambda (n) (treesit-node-match-p n thing t))))
          (progn
            (setq node parent-thing)
            (evil-set-jump))
        (setq node nil)))

    (when node
      (goto-char (1- (treesit-node-end node))))))



(defun evil-ts-obj--jump-param-boundaries ()
  (interactive)
  (when-let* ((thing evil-ts-obj-conf-nav-thing)
              (node (evil-ts-obj--thing-around (point) thing)))
    (cond
     ((= (treesit-node-start node) (point)) (goto-char (1- (treesit-node-end node))))
     ((= (1- (treesit-node-end node)) (point)) (goto-char (treesit-node-start node)))
     (t (goto-char (treesit-node-end node))))))

(defun evil-ts-obj--move-to-sibling (pos dir thing)
  (when-let ((dest (treesit--navigate-thing pos  dir 'beg thing 'nested)))
    ;; (evil-set-jump)
    (goto-char dest)))

(defun evil-ts-obj--current-thing (node dwim-thing)
  (pcase dwim-thing
    ((or (pred symbolp)
         (pred stringp))
     dwim-thing)
    ((pred listp)
     (seq-find (lambda (thing) (treesit-node-match-p node thing t))
               (cdr dwim-thing) nil))
    (_ (error "Unsupported dwim thing %s" dwim-thing))))

(defun evil-ts-obj--sibling-dwim (dir)
  (if-let* ((dwim-thing evil-ts-obj-conf-nav-dwim-thing)
            (node (evil-ts-obj--thing-around (point) dwim-thing))
            (cur-thing (evil-ts-obj--current-thing node dwim-thing)))
      ;; find next sibling of the thing we are currently at
      (evil-ts-obj--move-to-sibling (treesit-node-start node) dir cur-thing)
    ;; we are likely at root node; just move to next thing
    (evil-ts-obj--move-to-sibling (point) dir dwim-thing)))

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
      (evil-ts-obj-begin-of-thing 'compound-around)
    (evil-ts-obj-begin-of-thing evil-ts-obj-conf-nav-dwim-thing)))



;;;###autoload
(defun evil-ts-obj-end-of-thing-dwim ()
  (interactive)
  (if current-prefix-arg
      (evil-ts-obj-end-of-thing 'compound-around)
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
  (when-let* ((node (evil-ts-obj--thing-around (point) 'param-inner))
              (end-pos (treesit-node-end node)))
    (when-let ((sibling-or-sep (treesit-node-next-sibling node)))
      (if (not (equal (treesit-node-type sibling-or-sep) ","))
          ;; maybe lang without separators
          (setq end-pos (treesit-node-start sibling-or-sep))
        (setq end-pos (treesit-node-end sibling-or-sep))
        ;; try find next sibling param
        (setq sibling-or-sep (treesit-node-next-sibling sibling-or-sep))
        (when sibling-or-sep
          (setq end-pos (treesit-node-start sibling-or-sep)))))
    (list (treesit-node-start node)
          end-pos)))

;; TODO make a macro?
(evil-define-text-object evil-ts-obj-param-inner (count &optional _beg _end _type)
  "Select a param object."
  (when-let ((node (evil-ts-obj--thing-around (point) 'param-inner)))
    (if-let ((ext-func (alist-get (treesit-language-at (point)) evil-ts-obj-conf-param-inner-ext)))
        (funcall ext-func node)
      (list (treesit-node-start node)
            (treesit-node-end node)))))

(evil-define-text-object evil-ts-obj-compound-around (count &optional _beg _end _type)
  "Select a compound object."
  (when-let ((node (evil-ts-obj--thing-around (point) 'compound-around)))
    (if-let ((ext-func (alist-get (treesit-language-at (point)) evil-ts-obj-conf-compound-around-ext)) )
        (funcall ext-func node)
      (list (treesit-node-start node)
            (treesit-node-end node)))))

(evil-define-text-object evil-ts-obj-statement-around (count &optional _beg _end _type)
  "Select a compound object."
  (when-let ((node (evil-ts-obj--thing-around (point) 'statement-around)))
    (list (treesit-node-start node)
          (treesit-node-end node))))


(evil-define-text-object evil-ts-obj-compound-inner (count &optional _beg _end _type)
  "Select a compound inner object."
  (let ((node (evil-ts-obj--thing-around (point) 'compound-inner)))
    (list (treesit-node-start node)
          (treesit-node-end node))))


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
    (define-key map (kbd "s") #'evil-ts-obj-statement-around)
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
