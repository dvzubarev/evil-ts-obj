;;; evil-ts-obj-util.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Version: 0.0.1
;; Keywords: tools convenience
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides evil text-objects using tree-sitter
;;
;;; Code:


;; * paste utils

(defun evil-ts-obj-util--indent-text-according-to-point-pos (text &optional indent-empty-line)
  "Indent `TEXT' for later inserting to the buffer.
The functions should be called with the destination buffer as the
current buffer, and with point at the place where the string is
to be inserted."

  (let ((cur-indent (+ (current-indentation)
                       (- (point) (save-excursion
                                    (back-to-indentation)
                                    (point)))))
        (del-leading-spaces-for-first-line t))
    (when (and indent-empty-line (bolp) (eolp))
      ;; empty line
      (with-undo-amalgamate
        (indent-according-to-mode)
        (setq cur-indent (current-indentation)
              del-leading-spaces-for-first-line nil)
        (delete-region (pos-bol) (pos-eol))))
    (evil-ts-obj-util--pad-text cur-indent text del-leading-spaces-for-first-line)))

;; based on https://emacs.stackexchange.com/a/34981
(defun evil-ts-obj-util--pad-text (pad text &optional del-leading-spaces-for-first-line)
  "Indent the `TEXT' by the `PAD' amount.
If `DEL-LEADING-SPACES-FOR-FIRST-LINE' is t remove space before
the first line of TEXT."
  (let ((itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table))
        (indent nil))

    (with-temp-buffer
      (setq indent-tabs-mode itm
            tab-width tw)
      (set-syntax-table st)
      (insert text)
      ;; Establish the minimum level of indentation.
      (goto-char (point-min))
      (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                  (not (eobp)))
        (let ((length (current-column)))
          (when (or (not indent) (< length indent))
            (setq indent length)))
        (forward-line 1))
      (if (not indent)
          (error "Region is entirely whitespace")
        ;; Un-indent the buffer contents by the length of the minimum
        ;; indent level.

        (setq indent (- indent pad))

        (indent-rigidly (point-min) (point-max) (- indent))
        (when del-leading-spaces-for-first-line
          (goto-char (point-min))
          (skip-chars-forward " \t")
          (delete-region (point-min) (point)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun evil-ts-obj-util--extract-text (start end)
  "Extract text between START and END with first line indented."
  (let* ((text (buffer-substring-no-properties start end))
         (first-line-indent (evil-ts-obj-util--calc-first-line-indent start end))
         (prepared-text text))
    (when first-line-indent
      (setq prepared-text (concat (make-string first-line-indent 32) text)))
    prepared-text))

;;;###autoload
(defun evil-ts-obj-util--calc-first-line-indent (beg end)
  "Return number of chars before the first line when `BEG' `END'
span multiple lines."
  (save-excursion
    (goto-char beg)
    (when (< (pos-eol) end )
      ;; span multiple lines
      (+ (current-indentation)
         (- beg (progn (back-to-indentation) (point)))))))

;; * Misc utils

;; inspired by the code from map.el
;;;###autoload
(defmacro evil-ts-obj-util--pcase-plist-get (key map)
  "A macro to make MAP the last argument to `map-elt'."
  `(plist-get ,map ,key))

;;;###autoload
(pcase-defmacro pmap (&rest args)
  "Liki a map pattern, but only for plists."
  `(and (pred plistp)
        ,@(mapcar
           (lambda (elt)
             `(app (evil-ts-obj-util--pcase-plist-get ,(car elt)) ,(cadr elt)))
           args)))

(provide 'evil-ts-obj-util)
;;; evil-ts-obj-util.el ends here
