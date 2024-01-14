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


;;;  paste utils

(defun evil-ts-obj-util--indent-text-according-to-point-pos (text &optional indent-empty-line)
  "Indent `TEXT' for later inserting to the buffer.
The functions should be called with the destination buffer as the
current buffer, and with point at the place where the string will
be inserted. If INDENT-EMPTY-LINE is t and point is on empty
line, indent line according to mode and paste TEXT."

  (let ((cur-indent (current-column))
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
  "Indent the `TEXT' to `PAD' column rigidly.
Indent all lines in the TEXT forward by I = PAD - first-line-indent
columns (or backward if I is negative). This function only reindent
TEXT when the first line of TEXT has the same or lesser
indentation level as other lines. If
`DEL-LEADING-SPACES-FOR-FIRST-LINE' is t remove space before the
first line of TEXT."

  (let ((itm indent-tabs-mode)
        (tw tab-width)
        (st (syntax-table))
        (first-line-indent nil)
        (first-line-empty nil)
        (indent nil)
        (restore-trailing-spaces nil))

    (with-temp-buffer
      (setq indent-tabs-mode itm
            tab-width tw)
      (set-syntax-table st)
      (insert text)
      ;; Establish the minimum level of indentation.
      (goto-char (point-min))
      (setq first-line-empty (eolp))
      (while (and (re-search-forward "^[[:space:]\n]*" nil :noerror)
                  (not (eobp)))
        (let ((length (current-column)))
          (when (null first-line-indent)
            (setq first-line-indent length))
          (when (or (not indent) (< length indent))
            (setq indent length)))
        (forward-line 1))
      (if (not indent)
          (error "Region is entirely whitespace")
        (when (and (not first-line-empty)
                   (= first-line-indent indent))
          ;; First line has the same or lesser indentation level as other lines.
          ;; Most likely that indentation of other lines is dependent on the
          ;; indentation of the first line.
          ;; So adopt all lines to the new indentation level that is set via PAD parameter.
          ;; Otherwise do not indent, since lines seem to be indepedently indented.

          (setq indent (- pad indent))
          (when (and (eobp)
                     (save-excursion
                       (skip-chars-backward " \t")
                       (bolp)))
            ;; last line consist of spaces entirely
            ;; indent-rigidly will delete trailing spaces
            ;; we have to save here how many we have to restore
            (setq restore-trailing-spaces (current-column)))

          (indent-rigidly (point-min) (point-max) indent))

        (when del-leading-spaces-for-first-line
          (goto-char (point-min))
          (skip-chars-forward " \t")
          (delete-region (point-min) (point)))
        (when restore-trailing-spaces
          (goto-char (point-max))
          (insert (make-string restore-trailing-spaces 32)))
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
  "Return number of chars before the `BEG' position.
Return nil, if the range between `BEG', `END' does not span multiple lines."
  (save-excursion
    (goto-char beg)
    (when (and (not (eolp))
               (< (pos-eol) end ))
      ;; span multiple lines and not starts with empty line
      (current-column))))

;;;  Misc utils

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
