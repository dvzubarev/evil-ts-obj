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

;;; Customs and variables

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


;;; Basic edit function implementations
(defun evil-ts-obj-edit--replace (range target-range)
  "Replace text in TARGET-RANGE with then content within RANGE.
RANGE should be a cons of markers. Set last range to be the range
of inserted content. Last range is used by
`evil-ts-obj-last-range'."

  (let (text)
    (pcase-let ((`(,start . ,end) range))
      (with-current-buffer (marker-buffer start)
        (setq text (evil-ts-obj-util--extract-text start end))))
    (pcase-let ((`(,start . ,end) target-range))
      (with-current-buffer (marker-buffer start)
        (delete-region start end)

        (save-excursion
          (let ((init-pos (marker-position start)))
            (goto-char start)
            (insert (evil-ts-obj-util--indent-text-according-to-point-pos text))
            ;; after insert, marker is pushed to the end of inserted text.
            ;; We explicitly set this behavior when marker was created.
            ;; Make last range to be useful, setting it to inserted text range.
            (setq evil-ts-obj--last-text-obj-range (list init-pos (marker-position start)))))))))

(defun evil-ts-obj-edit--swap (range other-range)
  "Swap content of two ranges: RANGE and OTHER-RANGE."

  (let (text other-text save-first-to-last-range)
    ;; assign to range variable a range that is placed earlier in the buffer.
    (when (eq (marker-buffer (car range))
              (marker-buffer (car other-range)))
      (when (> (car range) (car other-range))
        (cl-rotatef range other-range)
        (setq save-first-to-last-range t))

      (when (< (car other-range) (cdr range))
        (user-error "Swap operator does not support intersected ranges!")))

    (pcase-let ((`(,ostart . ,oend) other-range))
      (with-current-buffer (marker-buffer ostart)
        (setq other-text (evil-ts-obj-util--extract-text ostart oend))
        ;; insert text from the other range into the beginning of the range
        (pcase-let ((`(,start . ,end) range))
          (with-current-buffer (marker-buffer start)
            (setq text (evil-ts-obj-util--extract-text start end))
            (save-excursion
              (let ((init-pos (marker-position start)))
                (goto-char start)
                (insert (evil-ts-obj-util--indent-text-according-to-point-pos other-text))
                (when save-first-to-last-range
                  (setq evil-ts-obj--last-text-obj-range (list init-pos (marker-position start))))
                (delete-region start end)))))
        ;; insert text from the range into the start of the other range
        (save-excursion
          (let ((init-pos (marker-position ostart)))
            (goto-char ostart)
            (insert (evil-ts-obj-util--indent-text-according-to-point-pos text))
            (when (not save-first-to-last-range)
              (setq evil-ts-obj--last-text-obj-range (list init-pos (marker-position ostart))))
            (delete-region ostart oend)))))))


;;; Helper functions

(defun evil-ts-obj-edit--thing-from-rules (rules-alist)
  (append '(or) (mapcar #'car rules-alist)))

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

;;; Operators

(defun evil-ts-obj-edit--replace-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'replace start end #'evil-ts-obj-edit--replace))

(defun evil-ts-obj-edit--swap-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'swap start end #'evil-ts-obj-edit--swap))

(defun evil-ts-obj-edit--raise-operator (start end)
  "Replace parent thing with the text from START END range.
Parent thing is determined by the cdr of `evil-ts-obj-conf-raise-rules'.
Actual raise is implemented via replace operator."

  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  ;; put current range to evil-ts-obj-edit--saved-range
  ;; as if it is the first call to replace operator
  (evil-ts-obj-edit--save-range-or-call-op 'raise start end #'evil-ts-obj-edit--replace)
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (raise-rules-func (plist-get evil-ts-obj-conf-raise-rules lang))
                  (last-spec evil-ts-obj--last-text-obj-spec)
                  (last-range evil-ts-obj--last-text-obj-range)
                  (rules-alist (funcall raise-rules-func 'place last-spec))
                  (parent-thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point)
                                                          parent-thing spec
                                                          last-range)))
        (evil-ts-obj-edit--save-range-or-call-op 'raise (car range) (cadr range)
                                                 #'evil-ts-obj-edit--replace))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--raise-dwim ()
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (raise-rules-func (plist-get evil-ts-obj-conf-raise-rules lang))
                  (rules-alist (funcall raise-rules-func 'text ))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec)))
        (evil-ts-obj-edit--raise-operator (car range) (cadr range)))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--drag (dir)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      ;; find first text object
      (when-let* ((lang (treesit-language-at (point)))
                  (drag-rules-func (plist-get evil-ts-obj-conf-drag-rules lang))
                  (rules-alist (funcall drag-rules-func 'first))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec nil t))
                  (node (caddr range)))
        (evil-ts-obj-edit--swap-operator (car range) (cadr range))
        ;; Find sibling to swap with.

        (when-let* ((last-spec evil-ts-obj--last-text-obj-spec)
                    (first-thing (plist-get last-spec :thing))
                    (second-rules-alist (funcall drag-rules-func 'second last-spec))
                    (second-thing (evil-ts-obj-edit--thing-from-rules second-rules-alist))
                    (second-spec (evil-ts-obj--make-spec second-rules-alist 'op))
                    (sibling (evil-ts-obj--find-matching-sibling node first-thing dir second-thing))
                    (sibling-range (evil-ts-obj--get-text-obj-range sibling second-thing second-spec)))
          (evil-ts-obj-edit--swap-operator (car sibling-range) (cadr sibling-range))
          (goto-char (car evil-ts-obj--last-text-obj-range))))
    (evil-ts-obj-edit--cleanup)))

(provide 'evil-ts-obj-edit)
;;; evil-ts-obj-edit.el ends here
