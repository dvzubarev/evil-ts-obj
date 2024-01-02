;;; evil-ts-obj-edit.el --- Description -*- lexical-binding: t; -*-
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

(require 'evil-ts-obj-util)
(require 'evil-ts-obj-core)

(defcustom evil-ts-obj-edit-highlight-face 'highlight
  "Face used to highlight the first selected range."
  :type 'face
  :group 'evil-ts-obj)

(defvar evil-ts-obj-edit--saved-range nil
  "This variable is used in operators that works with two ranges.
For example, replace, swap. This variable stores cons of the
operator type and the range that was selected during the first
invocation of the operator.")
(defvar evil-ts-obj-edit--overlays nil)


(defun evil-ts-obj-edit--replace (range target-range)
  "Replace text in TARGET-RANGE with then content within RANGE.
RANGE should be a cons of markers."

  (let (text)
    (pcase-let ((`(,start . ,end) range))
      (with-current-buffer (marker-buffer start)
        (setq text (evil-ts-obj-util--extract-text start end))))
    (pcase-let ((`(,start . ,end) target-range))
      (with-current-buffer (marker-buffer start)
        (delete-region start end)
        (save-excursion
          (goto-char start)
          (insert (evil-ts-obj-util--indent-text-according-to-point-pos text)))))))



(defun evil-ts-obj-edit--release-markers (range)
  (pcase-let ((`(,start . ,end) range))
    (set-marker start nil)
    (set-marker end nil)))

(defun evil-ts-obj-edit--cleanup (&optional second-range)
  (when evil-ts-obj-edit--saved-range
    (evil-ts-obj-edit--release-markers (cdr evil-ts-obj-edit--saved-range))
    (setq evil-ts-obj-edit--saved-range nil))
  (when second-range
    (evil-ts-obj-edit--release-markers second-range))

  (mapc 'delete-overlay evil-ts-obj-edit--overlays)
  (setq evil-ts-obj-edit--overlays nil))

(defun evil-ts-obj-edit-cancel ()
  (interactive)
  (evil-ts-obj-edit--cleanup))

(defun evil-ts-obj-edit--highlight-range (start end)
  (let ((o (make-overlay start end nil t nil)))
    (overlay-put o 'face evil-ts-obj-edit-highlight-face)
    (add-to-list 'evil-ts-obj-edit--overlays o)))

(defun evil-ts-obj-edit--save-range-or-call-op (op-type start end op-func)
  (let ((start-marker (copy-marker start t))
        (end-marker (copy-marker end nil)))
    (if (or (null evil-ts-obj-edit--saved-range)
            (not (eq op-type (car evil-ts-obj-edit--saved-range)))
            (not (buffer-live-p (marker-buffer (cadr evil-ts-obj-edit--saved-range)))))
        ;; first call with this op-type
        (progn
          (when evil-ts-obj-edit--saved-range
            (evil-ts-obj-edit--release-markers (cdr evil-ts-obj-edit--saved-range)))
          (setq evil-ts-obj-edit--saved-range (cons op-type (cons start-marker end-marker)))
          (evil-ts-obj-edit--highlight-range start end))
      ;; second call
      (let ((second-markers (cons start-marker end-marker)))
        (unwind-protect
            (funcall op-func
                     (cdr evil-ts-obj-edit--saved-range)
                     second-markers)
          (evil-ts-obj-edit--cleanup second-markers))))))

(defun evil-ts-obj-edit-replace-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'replace start end #'evil-ts-obj-edit--replace))



(provide 'evil-ts-obj-edit)
;;; evil-ts-obj-edit.el ends here
