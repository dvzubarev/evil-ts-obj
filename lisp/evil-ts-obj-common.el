;;; evil-ts-obj-common.el --- Description -*- lexical-binding: t; -*-
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

(require 'evil-ts-obj-conf)

;; * modifiers

(defun evil-ts-obj-common--find-next-sep-and-sibling (node)
  (let ((next-sibling (treesit-node-next-sibling node t))
        (next-sibling-or-sep (treesit-node-next-sibling node))
        (end-pos (treesit-node-end node))
        sep-found)

    (if next-sibling
        ;; end before next sibling parameter
        (setq end-pos (treesit-node-start next-sibling))
      (when next-sibling-or-sep
        (if (equal (treesit-node-type next-sibling-or-sep) evil-ts-obj-conf-param-sep)
            ;; a separator found
            (setq sep-found t
                  end-pos (treesit-node-end next-sibling-or-sep))
          ;; maybe a closing bracket
          (setq end-pos (treesit-node-start next-sibling-or-sep)))))


    (list end-pos next-sibling (when sep-found next-sibling-or-sep))))


(defun evil-ts-obj-common-param-outer-mod (node)
  (pcase-let* ((start-pos (treesit-node-start node))
               (`(,end-pos ,next-sibling ,next-sep)
                (evil-ts-obj-common--find-next-sep-and-sibling node)))

    (unless next-sibling
      ;; this is the last parameter
      ;; can we include previous separator?
      (when-let (((null next-sep))
                 (prev-sibling (treesit-node-prev-sibling node t)))

        ;; there was not trailing separator,
        ;; so we can assume that the previous one should be included
        (setq start-pos (treesit-node-end prev-sibling))))
    (list start-pos end-pos)))

(defun evil-ts-obj-common-param-upper-mod (node)
  (pcase-let* ((start-pos (treesit-node-start node))
               (`(,end-pos ,_ ,_)
                (evil-ts-obj-common--find-next-sep-and-sibling node)))
    (let ((final-sibling node))
      (while (setq node (treesit-node-prev-sibling node t))
        (setq final-sibling node))
      (setq start-pos (treesit-node-start final-sibling)))
    (list start-pos end-pos)))

(defun evil-ts-obj-common-param-lower-mod (node)
  (let ((start-pos (treesit-node-start node))
        end-pos)
    (when-let ((prev-sibling (treesit-node-prev-sibling node t)))
      (setq start-pos (treesit-node-end prev-sibling)))

    (let ((final-sibling node))
      (while (setq node (treesit-node-next-sibling node t))
        (setq final-sibling node))
      ;; check trailing sep
      (when-let* ((trailing-sep (treesit-node-next-sibling final-sibling))
                  ((equal (treesit-node-type trailing-sep) evil-ts-obj-conf-param-sep)))
        (setq final-sibling trailing-sep))
      (setq end-pos (treesit-node-end final-sibling)))
    (list start-pos end-pos)))

;; * paste utils

(defun evil-ts-obj-common--indent-text-according-to-point-pos (text)
  "Indent `TEXT' for later inserting to the buffer.
The functions should be called with the destination buffer as the
current buffer, and with point at the place where the string is
to be inserted."

  (let ((cur-indent (+ (current-indentation)
                       (- (point) (save-excursion
                                    (back-to-indentation)
                                    (point)))))
        (del-leading-spaces-for-first-line t))
    (when (and (bolp) (eolp))
      ;; empty line
      (with-undo-amalgamate
        (indent-according-to-mode)
        (setq cur-indent (current-indentation)
              del-leading-spaces-for-first-line nil)
        (delete-region (pos-bol) (pos-eol))))
    (evil-ts-obj-common--pad-text cur-indent text del-leading-spaces-for-first-line)))

;; based on https://emacs.stackexchange.com/a/34981
(defun evil-ts-obj-common--pad-text (pad text &optional del-leading-spaces-for-first-line)
  "Return string, un-indented by the length of its minimum indent.

If numeric prefix argument PAD is supplied, indent the resulting
text by that amount."
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

(defun evil-ts-obj-common--calc-first-line-indent (beg end)
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
(defmacro evil-ts-obj-common--pcase-plist-get (key map)
  "A macro to make MAP the last argument to `map-elt'."
  `(plist-get ,map ,key))

(pcase-defmacro pmap (&rest args)
  "Liki a map pattern, but only for plists."
  `(and (pred plistp)
        ,@(mapcar
           (lambda (elt)
             `(app (evil-ts-obj-common--pcase-plist-get ,(car elt)) ,(cadr elt)))
           args)))

(provide 'evil-ts-obj-common)
;;; evil-ts-obj-common.el ends here
