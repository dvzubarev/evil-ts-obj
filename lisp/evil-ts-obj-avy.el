;;; evil-ts-obj-avy.el --- Integration with Avy -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
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

(require 'generator)

(require 'avy)

(require 'evil-ts-obj-core)
(require 'evil-ts-obj-util)

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


(defvar evil-ts-obj-avy--current-select-spec nil)
(defvar evil-ts-obj-avy--current-spec nil)
(defvar evil-ts-obj-avy--selected-node nil)

(defvar evil-ts-obj-avy--current-marker nil
  "Go to this marker when evil operator is done.")
(defvar evil-ts-obj-avy--dont-jump-back-ops '(evil-change))

(defun evil-ts-obj-avy--set-selected-node (cand)
  (pcase-let ((`((,_ ,_ ,node) . ,_) cand))
    (setq evil-ts-obj-avy--selected-node node)))

(add-function :before avy-pre-action #'evil-ts-obj-avy--set-selected-node)



(defun evil-ts-obj-avy-action-goto (_pt)
  (when-let* ((node evil-ts-obj-avy--selected-node)
              (spec evil-ts-obj-avy--current-spec)
              (range (evil-ts-obj--apply-modifiers node (plist-get spec :thing) spec)))
    (goto-char (car range)))
  t)

(defun evil-ts-obj-avy--get-range-for-avy-action ()
  (let* ((node evil-ts-obj-avy--selected-node)
         (spec (plist-put evil-ts-obj-avy--current-spec :act 'op)))

    (evil-ts-obj--apply-modifiers node (plist-get spec :thing) spec)))

(defun evil-ts-obj-avy-action-delete-thing (_pt)
  (when-let ((range (evil-ts-obj-avy--get-range-for-avy-action)))
    (delete-region (car range) (cadr range)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun evil-ts-obj-avy-action-yank-thing (_pt)
  (when-let ((range (evil-ts-obj-avy--get-range-for-avy-action)))
    (copy-region-as-kill (car range) (cadr range)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)



(defun evil-ts-obj-avy--action-paste (&optional after delete-region)
  (when-let ((range (evil-ts-obj-avy--get-range-for-avy-action))
             (start (car range))
             (end (cadr range)))

    (let* ((text (evil-ts-obj-util--extract-text start end)))
      (when delete-region
        (delete-region start end))

      (select-window
       (cdr
        (ring-ref avy-ring 0)))

      (save-excursion
        (when (and after
                   (not (eolp)))
          (forward-char))
        (insert (evil-ts-obj-util--indent-text-according-to-point-pos text)))
      (skip-chars-forward " \t")))
  t)

(defun evil-ts-obj-avy-action-paste-after (_pt)
  (evil-ts-obj-avy--action-paste t))

(defun evil-ts-obj-avy-action-paste-before (_pt)
  (evil-ts-obj-avy--action-paste))

(defun evil-ts-obj-avy-action-teleport-after (_pt)
  (evil-ts-obj-avy--action-paste t t))

(defun evil-ts-obj-avy-action-teleport-before (_pt)
  (evil-ts-obj-avy--action-paste nil t))

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
                 (< (treesit-node-start cursor) end)
                 (>= (treesit-node-start cursor) start))
        (setq cursor (evil-ts-obj--propagate-to-identical-parent cursor thing))
        (iter-yield cursor)))

    ;; Since treesit-search-forward returns leafs first, we stopped on the first
    ;; leaf that starts aftr end. We should go up to its parents since they can
    ;; be inside range (start end).
    (while (and (setq cursor (treesit-node-parent cursor))
                (<= start (treesit-node-start cursor)))

      (when (and (< (treesit-node-start cursor) end)
                 (>= (treesit-node-start cursor) start )
                 (treesit-node-match-p cursor thing t))
        (setq cursor (evil-ts-obj--propagate-to-identical-parent cursor thing))
        (iter-yield cursor)))))


(defun evil-ts-obj-avy--get-candidates-current-window (thing)
  (let ((window (selected-window))
        (spec evil-ts-obj-avy--current-select-spec)
        range
        candidates)
    (pcase-dolist (`(,pos . ,end) (avy--find-visible-regions (window-start) (window-end)))
      ;; see https://lists.gnu.org/archive/html/bug-gnu-emacs/2023-12/msg01385.html
      (save-restriction
        (widen)
        (iter-do (node (evil-ts-obj-avy--iter-things thing pos end))
          (setq range (evil-ts-obj--apply-modifiers node thing spec))
          (push (cons (append range (list node))
                      window)
                candidates))))
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

(defun evil-ts-obj-avy--process-text-obj (thing mod &optional action)
  (let* ((avy-dispatch-alist evil-ts-obj-avy-dispatch-alist)
         (avy-action (or action avy-action-oneshot #'evil-ts-obj-avy-action-goto))
         (obj-act (if (or (bound-and-true-p evil-this-operator)
                          (region-active-p)
                          (not (memq avy-action '(evil-ts-obj-avy-action-goto identity))))
                      'op
                    'nav))
         (evil-ts-obj-avy--current-spec (evil-ts-obj--make-spec thing obj-act mod))
         (evil-ts-obj-avy--current-select-spec (evil-ts-obj--make-spec thing 'nav mod)))

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

(defun evil-ts-obj-avy-act-on-text-obj (thing mod)
  (when-let ((curr-comm (bound-and-true-p evil-this-operator)))
    (when (not (memq curr-comm evil-ts-obj-avy--dont-jump-back-ops))
      ;; have to setup hook for returning to the previous position after operator is done
      (setq evil-ts-obj-avy--current-marker (copy-marker (point)))
      (add-hook 'post-command-hook #'evil-ts-obj-avy--post-op-reset 10)))

  (setq evil-ts-obj-avy--selected-node nil)
  (avy-with evil-ts-obj-text-obj
    (evil-ts-obj-avy--process-text-obj thing mod))
  (when (and evil-ts-obj-avy--selected-node
             evil-ts-obj--last-text-obj-range)
    (copy-sequence evil-ts-obj--last-text-obj-range)))


(defmacro evil-ts-obj-avy-define-all-paste-cmds (thing key)
  "Define paste-after and teleport-after commands for a `THING'.
Also bind `KEY' to defined text objects in all appropriate keymaps."
  `(progn
     ,@(let (result)
         (dolist (mod '(outer inner upper lower))
           (let ((paste-map-name (intern (format "evil-ts-obj-avy-%s-paste-map" mod)))
                 (move-map-name (intern (format "evil-ts-obj-avy-%s-teleport-map" mod)))
                 (avy-cmd (intern (format "evil-ts-obj-avy-%s-%s-text-obj" thing mod)))
                 (paste-command (intern (format "evil-ts-obj-avy-%s-%s-paste-after" thing mod)))
                 (move-command (intern (format "evil-ts-obj-avy-%s-%s-teleport-after" thing mod))))
             (push `(declare-function ,avy-cmd nil) result)
             (push `(defun ,paste-command ()
                      ,(format "Paste remote %s %s thing behind the point." mod thing)
                      (interactive)
                      (let ((avy-action-oneshot #'evil-ts-obj-avy-action-paste-after))
                        (,avy-cmd)))
                   result)
             (push `(defun ,move-command ()
                      ,(format "Teleport remote %s %s thing behind the point." mod thing)
                      (interactive)
                      (let ((avy-action-oneshot #'evil-ts-obj-avy-action-teleport-after))
                        (,avy-cmd)))
                   result)
             (push `(keymap-set ,paste-map-name (kbd ,key) #',paste-command) result)
             (push `(keymap-set ,move-map-name (kbd ,key) #',move-command) result)))
         (nreverse result))))


(defvar evil-ts-obj-avy-inner-text-objects-map (make-sparse-keymap "Avy inner text objects"))
(defvar evil-ts-obj-avy-outer-text-objects-map (make-sparse-keymap "Avy outer text objects"))
(defvar evil-ts-obj-avy-upper-text-objects-map (make-sparse-keymap "Avy upper text objects"))
(defvar evil-ts-obj-avy-UPPER-text-objects-map (make-sparse-keymap "Avy UPPER text objects"))
(defvar evil-ts-obj-avy-lower-text-objects-map (make-sparse-keymap "Avy lower text objects"))
(defvar evil-ts-obj-avy-LOWER-text-objects-map (make-sparse-keymap "Avy LOWER text objects"))


(defvar evil-ts-obj-avy-inner-paste-map (make-sparse-keymap "Avy inner paste cmds"))
(defvar evil-ts-obj-avy-outer-paste-map (make-sparse-keymap "Avy outer paste cmds"))
(defvar evil-ts-obj-avy-upper-paste-map (make-sparse-keymap "Avy upper paste cmds"))
(defvar evil-ts-obj-avy-lower-paste-map (make-sparse-keymap "Avy lower paste cmds"))
(defvar evil-ts-obj-avy-inner-teleport-map (make-sparse-keymap "Avy inner teleport cmds"))
(defvar evil-ts-obj-avy-outer-teleport-map (make-sparse-keymap "Avy outer teleport cmds"))
(defvar evil-ts-obj-avy-upper-teleport-map (make-sparse-keymap "Avy upper teleport cmds"))
(defvar evil-ts-obj-avy-lower-teleport-map (make-sparse-keymap "Avy lower teleport cmds"))





;; * Misc commands

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


(provide 'evil-ts-obj-avy)
;;; evil-ts-obj-avy.el ends here
