;;; evil-ts-obj-avy.el --- Integration with Avy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
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

(require 'generator)

(require 'avy)

(require 'evil-ts-obj-core)
(require 'evil-ts-obj-util)

(defcustom evil-ts-obj-avy-key-prefix "z"
  "Default key binding for avy text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-avy-dispatch-alist
  '((?x . evil-ts-obj-avy-action-delete-thing)
    (?y . evil-ts-obj-avy-action-yank-thing)
    (?p . evil-ts-obj-avy-action-paste-after)
    (?P . evil-ts-obj-avy-action-paste-before)
    (?m . evil-ts-obj-avy-action-teleport-after)
    (?M . evil-ts-obj-avy-action-teleport-before))
  "Default actions for treesit objects."
  :type 'alist
  :group 'evil-ts-obj)


(defvar evil-ts-obj-avy--current-thing nil)
(defvar evil-ts-obj-avy--current-text-obj nil)
(defvar evil-ts-obj-avy--selected-node nil)
(defvar evil-ts-obj-avy--current-range nil
  "Last goto range.
Used for avy text objects. This range will be returned to the
evil operator.")

(defvar evil-ts-obj-avy--current-marker nil
  "Goto this marker when evil operator is done.")
(defvar evil-ts-obj-avy--dont-jump-back-ops '(evil-change))
(defvar evil-ts-obj-avy--activate-motion-range-advice nil)

(defun evil-ts-obj-avy--set-selected-node (cand)
  (pcase-let ((`((,_ ,_ ,node) . ,_) cand))
    (setq evil-ts-obj-avy--selected-node node)))

(add-function :before avy-pre-action #'evil-ts-obj-avy--set-selected-node)

(defun evil-ts-obj-avy--get-range (pos op-kind)
  (let* ((node evil-ts-obj-avy--selected-node)
         (thing evil-ts-obj-avy--current-thing)
         (text-obj evil-ts-obj-avy--current-text-obj)
         (spec (evil-ts-obj--make-spec op-kind thing text-obj)))
    (evil-ts-obj--apply-modifiers node thing spec)))

(defun evil-ts-obj-avy-action-goto (pt)
  (when-let ((range (evil-ts-obj-avy--get-range
                     pt
                     (if evil-this-operator 'mod 'nav))))
    (setq evil-ts-obj-avy--current-range range)
    (goto-char (car range)))
  t)

(defun evil-ts-obj-avy-action-delete-thing (pt)
  (when-let ((range (evil-ts-obj-avy--get-range pt 'mod)))
    (delete-region (car range) (cadr range)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun evil-ts-obj-avy-action-yank-thing (pt)
  (when-let ((range (evil-ts-obj-avy--get-range pt 'mod)))
    (copy-region-as-kill (car range) (cadr range)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)



(defun evil-ts-obj-avy--action-paste (pt &optional after delete-region)
  (when-let ((range (evil-ts-obj-avy--get-range pt 'mod))
             (start (car range))
             (end (cadr range)))

    (let* ((text (buffer-substring-no-properties start end))
           (first-line-indent (evil-ts-obj-util--calc-first-line-indent start end))
           (prepared-text text))
      (when first-line-indent
        (setq prepared-text (concat (make-string first-line-indent 32) text)))

      (when delete-region
        (delete-region start end))

      (select-window
       (cdr
        (ring-ref avy-ring 0)))

      (save-excursion
        (when (and after
                   (not (eolp)))
          (forward-char))
        (insert (evil-ts-obj-util--indent-text-according-to-point-pos prepared-text)))
      (skip-chars-forward " \t")))
  t)

(defun evil-ts-obj-avy-action-paste-after (pt)
  (evil-ts-obj-avy--action-paste pt t))

(defun evil-ts-obj-avy-action-paste-before (pt)
  (evil-ts-obj-avy--action-paste pt))

(defun evil-ts-obj-avy-action-teleport-after (pt)
  (evil-ts-obj-avy--action-paste pt t t))

(defun evil-ts-obj-avy-action-teleport-before (pt)
  (evil-ts-obj-avy--action-paste pt nil t))

;; see https://github.com/abo-abo/avy/pull/371
(defun avy--visible-p (s)
  (let ((prop (get-char-property s 'invisible)))
    (not (if (eq buffer-invisibility-spec t)
             prop
           (or (memq prop buffer-invisibility-spec)
               (assq prop buffer-invisibility-spec))))))

(iter-defun evil-ts-obj-avy--iter-things (thing start end)
  (let ((cursor (treesit-node-at start)))
    (while (and cursor
                (< (treesit-node-start cursor) end))
      (setq cursor
            (treesit-search-forward
             cursor
             (lambda (n) (treesit-node-match-p n thing t))))
      (when (and cursor
                 (< (treesit-node-start cursor) end))
        (iter-yield cursor)))

    ;; Since treesit-search-forward returns leafs first, we stopped on the first
    ;; leaf that starts aftr end. We should go up to its parents since they can
    ;; be inside range (start end).
    (while (and (setq cursor (treesit-node-parent cursor))
                (<= start (treesit-node-start cursor)))

      (when (and (< (treesit-node-start cursor) end)
                 (treesit-node-match-p cursor thing t))
        (iter-yield cursor)))))


(defun evil-ts-obj-avy--get-candidates-current-window (thing)
  (let ((window (selected-window))
        (spec (evil-ts-obj--make-spec 'avy-cand
                                      thing
                                      evil-ts-obj-avy--current-text-obj))
        range
        candidates)
    (pcase-dolist (`(,pos . ,end) (avy--find-visible-regions (window-start) (window-end)))
      (iter-do (node (evil-ts-obj-avy--iter-things thing pos end))
        (setq range (evil-ts-obj--apply-modifiers node thing spec))
        (push (cons (append range (list node))
               window)
              candidates)))
    candidates))

(defun evil-ts-obj-avy--collect-candidates (thing)
  (let ((candidates nil))
    (avy-dowindows nil
      (when (treesit-parser-list)
        ;; treesit is enabled in this buffer
        (setq candidates
              (append
               candidates
               (evil-ts-obj-avy--get-candidates-current-window thing)))))
    candidates))

(defun evil-ts-obj-avy-on-thing (thing &optional action)
  (let ((avy-dispatch-alist evil-ts-obj-avy-dispatch-alist)
        (avy-action (or action avy-action-oneshot #'evil-ts-obj-avy-action-goto)))
    (when-let ((candidates (evil-ts-obj-avy--collect-candidates thing)))
      (avy-process candidates))))

(defun evil-ts-obj-avy--post-op-reset ()
  (when-let* ((marker evil-ts-obj-avy--current-marker)
              (buf (marker-buffer marker))
              (pos (marker-position marker)))
    (when (not (eq buf (current-buffer)))
      (select-window (get-buffer-window buf)))
    (goto-char pos)
    (set-marker marker nil)
    (setq evil-ts-obj-avy--current-marker nil))
  (remove-hook 'post-command-hook #'evil-ts-obj-avy--post-op-reset))


(defun evil-ts-obj-avy--evil-motion-advice (&rest _r)
  "This advice prevents evil-motion-range from restoring point after
motion is executed.
It is needed so evil operators can work on
text objects in in other windows."

  (when evil-ts-obj-avy--activate-motion-range-advice
    (setq evil-ts-obj-avy--activate-motion-range-advice nil
          evil-inhibit-operator nil)))

(advice-add 'evil-motion-range :after #'evil-ts-obj-avy--evil-motion-advice)

(defmacro evil-ts-obj-avy-define-text-obj (thing text-obj)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-avy-%s-%s-text-obj" thing text-obj))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       (when evil-this-operator
         (setq evil-ts-obj-avy--activate-motion-range-advice t
               evil-inhibit-operator t)

         (when (not (memq evil-this-operator evil-ts-obj-avy--dont-jump-back-ops))
           ;; have to setup hook for returning to the previous position after operator is done
           (setq evil-ts-obj-avy--current-marker (copy-marker (point)))
           (add-hook 'post-command-hook #'evil-ts-obj-avy--post-op-reset 10)))

       (setq evil-ts-obj-avy--current-range nil)
       (avy-with avy-compound-inner
         (let ((evil-ts-obj-avy--current-thing ',thing)
               (evil-ts-obj-avy--current-text-obj ',text-obj))
           (evil-ts-obj-avy-on-thing ',thing)))
       (if evil-ts-obj-avy--current-range
           evil-ts-obj-avy--current-range
         ;; Return an empty range so evil-motion-range doesn't try to guess
         (let ((p (point)))
           (list p p 'exclusive))))))

(evil-ts-obj-avy-define-text-obj compound outer)
(evil-ts-obj-avy-define-text-obj compound inner)
(evil-ts-obj-avy-define-text-obj compound upper)
(evil-ts-obj-avy-define-text-obj compound lower)

(defun evil-ts-obj-avy-compound-outer-paste-after ()
  (interactive)
  (let ((avy-action-oneshot #'evil-ts-obj-avy-action-paste-after))
    (evil-ts-obj-avy-compound-outer-text-obj nil)))

(evil-ts-obj-avy-define-text-obj statement outer)
(evil-ts-obj-avy-define-text-obj statement inner)
(evil-ts-obj-avy-define-text-obj statement upper)
(evil-ts-obj-avy-define-text-obj statement lower)

(defun evil-ts-obj-avy-statement-outer-paste-after ()
  (interactive)
  (let ((avy-action-oneshot #'evil-ts-obj-avy-action-paste-after))
    (evil-ts-obj-avy-statement-outer-text-obj nil)))

(evil-ts-obj-avy-define-text-obj param outer)
(evil-ts-obj-avy-define-text-obj param inner)
(evil-ts-obj-avy-define-text-obj param upper)
(evil-ts-obj-avy-define-text-obj param lower)

(defun evil-ts-obj-avy-param-inner-paste-after ()
  (interactive)
  (let ((avy-action-oneshot #'evil-ts-obj-avy-action-paste-after))
    (evil-ts-obj-avy-param-inner-text-obj nil)))


;;;###autoload
(defun evil-ts-obj-avy-jump-by-query (query &optional action)
  (interactive)
  (setq avy-action (or action #'avy-action-goto))
  (when-let ((root (treesit-buffer-root-node)))

    (avy-process
     (mapcar
      (lambda (c) (cons c (selected-window)))
      ;; treesit returns objects that are not visible but are children of a visible top-level thing
      ;; filter them
      (seq-filter (lambda (pair) (and (>= (car pair) (window-start))))
                  (treesit-query-range root query (window-start) (window-end)))))))

(defvar evil-ts-obj-avy-inner-text-objects-map
  (let ((map (make-sparse-keymap "Inner text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-avy-compound-inner-text-obj)
    (define-key map (kbd "a") #'evil-ts-obj-avy-param-inner-text-obj)
    (define-key map (kbd "s") #'evil-ts-obj-avy-statement-inner-text-obj)
    map))
(defvar evil-ts-obj-avy-outer-text-objects-map
  (let ((map (make-sparse-keymap "Outer text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-avy-compound-outer-text-obj)
    (define-key map (kbd "a") #'evil-ts-obj-avy-param-outer-text-obj)
    (define-key map (kbd "s") #'evil-ts-obj-avy-statement-outer-text-obj)
    map))
(defvar evil-ts-obj-avy-upper-text-objects-map
  (let ((map (make-sparse-keymap "Upper text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-avy-compound-upper-text-obj)
    (define-key map (kbd "a") #'evil-ts-obj-avy-param-upper-text-obj)
    (define-key map (kbd "s") #'evil-ts-obj-avy-statement-upper-text-obj)
    map))
(defvar evil-ts-obj-avy-lower-text-objects-map
  (let ((map (make-sparse-keymap "Lower text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-avy-compound-lower-text-obj)
    (define-key map (kbd "a") #'evil-ts-obj-avy-param-lower-text-obj)
    (define-key map (kbd "s") #'evil-ts-obj-avy-statement-lower-text-obj)
    map))


(evil-define-key '(normal operator visual) evil-ts-obj-mode-map
  (kbd (concat evil-ts-obj-avy-key-prefix " i")) evil-ts-obj-avy-inner-text-objects-map
  (kbd (concat evil-ts-obj-avy-key-prefix " a")) evil-ts-obj-avy-outer-text-objects-map
  (kbd (concat evil-ts-obj-avy-key-prefix " u")) evil-ts-obj-avy-upper-text-objects-map
  (kbd (concat evil-ts-obj-avy-key-prefix " o")) evil-ts-obj-avy-lower-text-objects-map)



(provide 'evil-ts-obj-avy)
;;; evil-ts-obj-avy.el ends here
