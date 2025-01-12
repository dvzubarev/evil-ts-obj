;;; evil-ts-obj-edit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
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
(defun evil-ts-obj-edit--replace (range target-range &optional keep-props)
  "Replace text in TARGET-RANGE with then content within RANGE.
RANGE should be a cons of markers. Return the range of the
inserted content. Set KEEP-PROPS to preserve text properties of
the text in RANGE."

  (let (text)
    (pcase-let ((`(,start . ,end) range))
      (with-current-buffer (marker-buffer start)
        (setq text (evil-ts-obj-util--extract-text start end keep-props))))
    (pcase-let ((`(,start . ,end) target-range))
      (with-current-buffer (marker-buffer start)
        (delete-region start end)

        (save-excursion
          (let ((init-pos (marker-position start)))
            (goto-char start)
            (insert (evil-ts-obj-util--indent-text-according-to-point-pos text))
            ;; after insert, marker is pushed to the end of inserted text.
            ;; We explicitly set this behavior when marker was created.
            (list init-pos (marker-position start))))))))

(defun evil-ts-obj-edit--simple-replace (range-or-text target-range &optional keep-props)
  "Replace text in TARGET-RANGE with then content within RANGE-OR-TEXT.
If RANGE-OR-TEXT is not cons then it should be string.
RANGE-OR-TEXT and TARGET-RANGE should be in the same buffer. For
the more generic version see `evil-ts-obj-edit--replace' Set
KEEP-PROPS to preserve text properties of the text in RANGE."
  (let ((text (if (stringp range-or-text)
                  range-or-text
                (evil-ts-obj-util--extract-text (car range-or-text) (cdr range-or-text) keep-props))))
    (pcase-let ((`(,start . ,end) target-range))
      (delete-region start end)
      (save-excursion
        (goto-char start)
        (insert (evil-ts-obj-util--indent-text-according-to-point-pos text))))))

(defun evil-ts-obj-edit--swap (range other-range)
  "Swap content of two ranges: RANGE and OTHER-RANGE.
Return new range of the content from the RANGE."

  (let (text other-text swap-final-ranges
             final-range final-other-range)
    ;; assign to range variable a range that is placed earlier in the buffer.
    (when (eq (marker-buffer (car range))
              (marker-buffer (car other-range)))
      (when (> (car range) (car other-range))
        (cl-rotatef range other-range)
        (setq swap-final-ranges t))

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
                (setq final-other-range (list init-pos (marker-position start)))
                (delete-region start end)))))
        ;; insert text from the range into the start of the other range
        (save-excursion
          (let ((init-pos (marker-position ostart)))
            (goto-char ostart)
            (insert (evil-ts-obj-util--indent-text-according-to-point-pos text))
            (setq final-range (list init-pos (marker-position ostart)))
            (delete-region ostart oend)))))
    (if swap-final-ranges
        final-other-range
      final-range)))

(defun evil-ts-obj-edit--clone-compatible? (text-last-char place-first-char)
  "Heuristic to determine whether to insert a space between texts.
Return t if one of the chars (TEXT-LAST-CHAR or PLACE-FIRST-CHAR)
is newline, space or punctuation."
  (when (and text-last-char place-first-char)
    (let ((wh-chars '(?> ?\  ?.))
          (chr-syn (char-syntax text-last-char))
          (pl-syn (char-syntax place-first-char)))
      (or (memq chr-syn wh-chars)
          (memq pl-syn wh-chars)
          (eq pl-syn ?\))))))

(defun evil-ts-obj-edit--clone-indent (indent-ref-pos &optional start end)
  "Indent current point.
Indentation level is controlled via
`evil-ts-obj-conf-clone-indent-policy' variable. If value of this
variable is cur-indent, then indentation is equal to current
indentation at INDENT-REF-POS position. If its value is column,
then current column at INDENT-REF-POS is used as indentation
level. If this variable is nil, then `indent-according-to-mode'
is used. If START and END are not nil, then indent this region
instead of point."
  (let* ((indent-policy (plist-get evil-ts-obj-conf-clone-indent-policy
                                   (treesit-language-at indent-ref-pos)))
         indent)
    (save-excursion
      (goto-char indent-ref-pos)
      (setq indent
            (pcase indent-policy
              ('column (current-column))
              ('cur-indent (current-indentation)))))
    (if (null start)
        (if indent
            (indent-to indent)
          (indent-according-to-mode))

      (if (null indent)
          (indent-region start end)
        (indent-rigidly start end (- indent (current-indentation)))))))

(defun evil-ts-obj-edit--clone-check-ranges (text-range place-range delete-range after)
  (when delete-range
    (if after
        (when (< (car text-range) (cdr place-range) (cdr text-range))
          (user-error "Teleporting of a text range that contains place point!"))
      (when (< (car text-range) (car place-range) (cdr text-range))
        (user-error "Teleporting of a text range that contains place point!")))))

(defun evil-ts-obj-edit--find-compound-brackets (brackets range)
   (when (and (= (length brackets) 2)
         (eq (char-syntax (aref brackets 0)) ?\()
         (eq (char-syntax (aref brackets 1)) ?\)))
    ;; for example {}
    (save-excursion
      (and (progn
             (goto-char (car range))
             (skip-chars-backward "\t \n")
             (eq (char-before) (aref brackets 0)))
           (progn
             (goto-char (cadr range))
             (skip-chars-forward "\t \n")
             (eq (char-after) (aref brackets 1)))))))


(defun evil-ts-obj-edit--delete-compound-inner (range)
  "Delete content inside RANGE and maybe insert placeholder statement.
Placeholder is only inserted if the deleted range is equal to
current compound inner text object."

  (when (markerp (car range))
    (setq range (list (marker-position (car range))
                      (marker-position (cdr range)))))


  (let* ((lang (treesit-language-at (point)))
         (placeholders (plist-get evil-ts-obj-conf-statement-placeholder lang))
         (brackets (plist-get evil-ts-obj-conf-compound-brackets lang)))

    (if-let* (((or placeholders brackets))
              (spec (evil-ts-obj--make-spec 'compound 'op 'outer))
              (temp-range (evil-ts-obj--get-text-obj-range (car range)
                                                           'compound spec range t))
              (compound-inner-range (evil-ts-obj--apply-modifiers (caddr temp-range) 'compound
                                                                  (plist-put spec :mod 'inner) t))
              ((equal range compound-inner-range)))
        ;; delete-range and insert placeholder
        (let ((insert-text
               (if placeholders (car placeholders)
                 (unless (evil-ts-obj-edit--find-compound-brackets brackets range)
                   brackets))))
          (delete-region (car range) (cadr range))
          (when insert-text
            (save-excursion
              (goto-char (car range))
              (insert insert-text))))
      ;; just delete region
      (delete-region (car range) (cadr range)))))

(defun evil-ts-obj-edit--collect-text-feats (text-range)
  "Collect number of features for the text represented by TEXT-RANGE.
TEXT-RANGE should be a cons of markers. This function returns alist with
the following fields: * text-starts-with-space - t if the first
character of the text has space syntax category. * text-first-line-empty
- t if the first line of text contains only characters with space or
escape syntax category. * text-first-char - first character of the text.
* text-on-bol - t if between the first character of the text is at the
bol or if between it and bol are only characters with space syntax
category. Also there are similar features for the end of the text:
text-on-eol, text-last-line-empty, text-last-char."
  (let (text-starts-with-space
        text-first-line-empty
        text-last-line-empty
        text-first-char
        text-last-char
        text-bol
        newline-after-text)
    (pcase-let ((`(,start . ,end) text-range))
      (with-current-buffer (marker-buffer start)
        (save-excursion
          (goto-char end)
          (setq text-last-char (char-after (1- (point)))
                newline-after-text (eq (char-after) ?\n))
          (skip-syntax-backward " ")
          (setq text-last-line-empty (bolp))
          (goto-char start)
          (setq text-starts-with-space (eq (char-syntax (char-after)) ?\ )
                text-first-char (char-after))
          (skip-syntax-forward " \\")
          (setq text-first-line-empty (eolp))

          (goto-char start)
          (skip-syntax-backward " ")
          (setq text-bol (bolp))))

      `((text-starts-with-space . ,text-starts-with-space)
        (text-first-line-empty . ,text-first-line-empty)
        (text-first-char . ,text-first-char)
        (text-on-bol . ,text-bol)
        (text-last-char . ,text-last-char)
        (text-last-line-empty . ,text-last-line-empty)
        (text-on-eol . ,newline-after-text)))))

(defun evil-ts-obj-edit--handle-cloned-orig-text (text-range op-type)
  "Finalize cloned text represented by TEXT-RANGE.
TEXT-RANGE is the cons of markers. This function is meant to be invoked
during clone edit operations. Values of OP-TYPE: nil, t, comment, safe.
When OP-TYPE is nil text is left untouched.
Otherwise some function is invoked on provided TEXT-RANGE:
- safe: `evil-ts-obj-edit--delete-compound-inner',
- comment: `comment-region',
- t: `delete-region'."
  (pcase-let ((`(,start . ,end) text-range))
    (with-current-buffer (marker-buffer start)
      (pcase op-type
        ('safe (evil-ts-obj-edit--delete-compound-inner text-range))
        ('comment (comment-region start end))
        ('t (delete-region start end))))))

(defun evil-ts-obj-edit--clone-after (text-range place-range &optional delete-range)
  "Copy content of TEXT-RANGE to the position after PLACE-RANGE.
Return new range of the cloned content. If the end of the
PLACE-RANGE is \n and content does not start with newline, then
new line is inserted and indented according to an indentation at
the beginning of the PLACE-RANGE (see
`evil-ts-obj-edit--clone-indent'). After this content of RANGE is
inserted on the new line.

Possible values of DELETE-RANGE are t, nil and safe or comment symbols.
See `evil-ts-obj-edit--handle-cloned-orig-text' for more information."

  (evil-ts-obj-edit--clone-check-ranges text-range place-range delete-range t)
  (let-alist (evil-ts-obj-edit--collect-text-feats text-range)
    (pcase-let ((`(,start . ,end) place-range)
                (`(,text-start . ,text-end) text-range)
                (result-start (make-marker))
                (result-end (make-marker)))
      (with-current-buffer (marker-buffer start)
        (let ((insert-pos (marker-position end))
              (place-last-char (char-after (1- end)))
              (text (if .text-starts-with-space
                        (buffer-substring-no-properties text-start text-end)
                      (evil-ts-obj-util--extract-text text-start text-end))))
          (save-excursion
            (goto-char insert-pos)
            (cond
             ;; {} - encloses inserted text
             ;; 1st case:
             ;; place{\n
             ;; text}
             ((and (eq (char-after insert-pos) ?\n)
                   ;; heuristics to determine whether to open a new line
                   (not .text-first-line-empty)
                   (not (eql (marker-position start) (marker-position end)))
                   (or
                    ;; before text was only spaces
                    .text-on-bol
                    ;; place ends on punct charachter
                    (eq (char-syntax place-last-char) ?.)
                    ;; place last charachter is not "compatible" with the text first one
                    (not (evil-ts-obj-edit--clone-compatible? place-last-char .text-first-char))))
              ;; Insert new line and insert on it.
              ;; Use indentation of the start position.
              (newline)
              (evil-ts-obj-edit--clone-indent start)
              (end-of-line)
              (setq insert-pos (point)))
             ;; 2nd case:
             ;; place{ text}
             ((not (evil-ts-obj-edit--clone-compatible? place-last-char .text-first-char))
              (insert " ")
              (setq insert-pos (point)))
             ;; 3rd case:
             ;; place\n
             ;; {text
             ;; \n}afterplace
             ((not (evil-ts-obj-edit--clone-compatible? (aref text (1- (length text)))
                                                        (char-after end)))
              ;; need to insert a space before the text that is next to insert point
              (save-excursion
                (if (not .text-on-eol)
                    (insert " ")
                  (insert (concat "\n" (make-string (current-column) 32)))))))

            (let ((indented-text (if (not .text-starts-with-space)
                                     (evil-ts-obj-util--indent-text-according-to-point-pos text)
                                   text)))
              (insert indented-text)
              ;; Set markers here so they will be adjusted after handle-cloned-orig-text function.
              (set-marker result-start insert-pos)
              (set-marker result-end (+ insert-pos (length indented-text)))))))

      (evil-ts-obj-edit--handle-cloned-orig-text text-range delete-range)

      ;; Return the range of newly inserted text.
      (prog1
          (list (marker-position result-start)
                (marker-position result-end))
        (set-marker result-start nil)
        (set-marker result-end nil)))))

(defun evil-ts-obj-edit--clone-before (text-range place-range &optional delete-range)
  "Copy content of TEXT-RANGE before the beginning of PLACE-RANGE.
Return new range of the cloned content. If beginning of the
PLACE-RANGE preceded by only spaces and new content does not end
with newline, then a new line is inserted and indented according
to an indentation at the beginning of the PLACE-RANGE (see
`evil-ts-obj-edit--clone-indent'). After this content of RANGE is
inserted on the new line.

Possible values of DELETE-RANGE are t, nil and safe or comment symbols.
See `evil-ts-obj-edit--handle-cloned-orig-text' for more information."
  (evil-ts-obj-edit--clone-check-ranges text-range place-range delete-range nil)
  (let-alist (evil-ts-obj-edit--collect-text-feats text-range)
    (pcase-let* ((`(,start . ,end) place-range)
                 (`(,text-start . ,text-end) text-range)
                 (text (if .text-starts-with-space
                           (buffer-substring-no-properties text-start text-end)
                         (evil-ts-obj-util--extract-text text-start text-end)))
                 (result-start (make-marker))
                 (result-end (make-marker)))
      (with-current-buffer (marker-buffer start)
        (let ((insert-pos (marker-position start))
              (point-mark (copy-marker (max start (point)) t))
              indent)
          (goto-char insert-pos)
          (cond
           ;; {} - encloses inserted text
           ;; 1st case:
           ;; {text\n}
           ;; place
           ;; heuristics to determine whether to open a new line
           ((and (not (eolp))
                 (save-excursion (skip-chars-backward " \t") (bolp))
                 (not .text-last-line-empty))

            ;; Insert point is at the start of the line.
            ;; Insert new line with the indentation of the start position.
            (goto-char (line-beginning-position))
            (newline)
            (forward-line -1)
            (evil-ts-obj-edit--clone-indent start)
            (end-of-line)
            (setq insert-pos (point)))
           ;; 2nd case:
           ;;     beforeplace {text\n}
           ;;     place
           ((and
             .text-on-eol
             (or (eq (char-syntax .text-last-char) ?.)
                 (not (evil-ts-obj-edit--clone-compatible? .text-last-char (char-after start)))))
            ;; Move current content down one line
            (setq indent (current-column))
            (open-line 1)
            (setq insert-pos (point))
            (forward-line 1)
            (set-marker-insertion-type start nil)
            (indent-to indent)
            (evil-ts-obj-edit--clone-indent insert-pos start end)
            (goto-char insert-pos))
           ;; 3rd case;
           ;; {text }place
           ((not (evil-ts-obj-edit--clone-compatible? .text-last-char (char-after start)))
            (save-excursion
              (insert " "))))

          (let ((indented-text (if (not .text-starts-with-space)
                                   (evil-ts-obj-util--indent-text-according-to-point-pos text)
                                 text)))
            (insert indented-text)
            ;; Set markers here so they will be adjusted after handle-cloned-orig-text function.
            (set-marker result-start insert-pos)
            (set-marker result-end (+ insert-pos (length indented-text))))


          ;; Handle text from original buffer.
          (evil-ts-obj-edit--handle-cloned-orig-text text-range delete-range)

          (goto-char point-mark)
          (set-marker point-mark nil)
          (prog1
              (list (marker-position result-start) (marker-position result-end))
            (set-marker result-start nil)
            (set-marker result-end nil)))))))


;;; Helper functions

(defun evil-ts-obj-edit--thing-from-rules (rules-alist &rest extra-things)
  ""
  (append '(or) (mapcar #'car rules-alist) extra-things))

(defun evil-ts-obj-edit--release-markers (range)
  (pcase-let ((`(,start . ,end) range))
    (set-marker start nil)
    (set-marker end nil)))

(defun evil-ts-obj-edit--cleanup (&optional second-range)
  (when evil-ts-obj-edit--saved-range
    (evil-ts-obj-edit--release-markers (nth 1 evil-ts-obj-edit--saved-range))
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

(defun evil-ts-obj-edit--save-range-or-call-op (op-type start end op-func &rest args)
  "Helper function for two-staged operators.
If invoked for the first time, saves START, END and OP-TYPE to
the `evil-ts-obj-edit--saved-range'. If
`evil-ts-obj-edit--saved-range' is not nil and OP-TYPE is equal
to saved op-type, then invokes OP-FUNC on two ranges. It expects
that OP-FUNC returns range that is set to
`evil-ts-obj--last-text-obj-range' variable. Last range is used
by `evil-ts-obj-last-range'."
  (let ((start-marker (copy-marker start t))
        (end-marker (copy-marker end nil)))
    (if (or (null evil-ts-obj-edit--saved-range)
            (not (eq op-type (car evil-ts-obj-edit--saved-range)))
            (not (buffer-live-p (marker-buffer (car (nth 1 evil-ts-obj-edit--saved-range))))))
        ;; It is the first call with this op-type.
        (progn
          (evil-ts-obj-edit--cleanup)
          (setq evil-ts-obj-edit--saved-range (list op-type (cons start-marker end-marker) args))
          (evil-ts-obj-edit--highlight-range start end)
          nil)
      ;; The second call.
      (let ((second-markers (cons start-marker end-marker)))
        (unwind-protect
            (setq evil-ts-obj--last-text-obj-range
                  (apply op-func
                         (nth 1 evil-ts-obj-edit--saved-range)
                         second-markers
                         (pcase args
                           ((or '() '(nil)) (nth 2 evil-ts-obj-edit--saved-range))
                           (_ args))))
          (evil-ts-obj-edit--cleanup second-markers))))))

;;; Operators

(defun evil-ts-obj-edit--replace-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'replace start end #'evil-ts-obj-edit--replace))

(defun evil-ts-obj-edit--swap-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'swap start end #'evil-ts-obj-edit--swap))

(defun evil-ts-obj-edit--clone-after-operator (start end &optional comment?)
  ""
  (evil-ts-obj-edit--save-range-or-call-op
   'clone start end
   (lambda (r or dr)
     (let ((res (evil-ts-obj-edit--clone-after r or dr)))
       ;; Move point to the start of inserted text.
       (when (eq dr 'comment)
         (goto-char (car res)))
       res))
   (when comment? 'comment)))

(defun evil-ts-obj-edit--teleport-after-operator (start end &optional del-type)
  (evil-ts-obj-edit--save-range-or-call-op
   'teleport start end
   (lambda (r or) (evil-ts-obj-edit--clone-after r or (or del-type t)))))

(defun evil-ts-obj-edit--clone-before-operator (start end &optional comment?)
  ""
  (evil-ts-obj-edit--save-range-or-call-op
   'clone start end
   (lambda (r or dr)
     (let ((res (evil-ts-obj-edit--clone-before r or dr)))
       ;; Move point to the start of the inserted text.
       (when (eq dr 'comment)
         (goto-char (car res)))
       res))
   (when comment? 'comment)))

(defun evil-ts-obj-edit--teleport-before-operator (start end &optional del-type)
  (evil-ts-obj-edit--save-range-or-call-op
   'teleport start end
   (lambda (r or) (evil-ts-obj-edit--clone-before r or (or del-type t)))))


;;;; Raise
(defun evil-ts-obj-edit--raise-operator (start end &optional count)
  "Replace parent thing with the text from START END range.
Parent thing is determined by the `evil-ts-obj-conf-raise-rules'
variable. When COUNT is set select Nth parent. Actual raise is
implemented via replace operator."

  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  ;; Put current range to `evil-ts-obj-edit--saved-range'
  ;; as if it is the first call to replace operator.
  (evil-ts-obj-edit--save-range-or-call-op 'raise start end #'evil-ts-obj-edit--replace)
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (raise-rules-func (plist-get evil-ts-obj-conf-raise-rules lang))
                  (text-range (list start end))
                  (place-rules-alist (funcall raise-rules-func 'place))
                  (place-thing (evil-ts-obj-edit--thing-from-rules place-rules-alist))
                  (place-spec (evil-ts-obj--make-spec place-rules-alist 'op))
                  (place-range (evil-ts-obj--get-text-obj-range (point)
                                                                place-thing place-spec
                                                                text-range t)))
        (when-let* ((count (or count 1))
                    ((> count 1))
                    (node (caddr place-range))
                    (parent node))
          (cl-dotimes (_ (1- count))
            (if (setq parent (treesit-parent-until node place-thing))
                (setq node parent)
              (cl-return node)))
          (setq place-range (evil-ts-obj--get-text-obj-range node place-thing place-spec)))


        (evil-ts-obj-edit--save-range-or-call-op 'raise (car place-range) (cadr place-range)
                                                 #'evil-ts-obj-edit--replace))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--raise-dwim (&optional count)
  "Replace parent text object with the current text object at point.
Current and parent text objects are determined by the
`evil-ts-obj-conf-raise-rules' variable. When COUNT is set select
Nth parent."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (raise-rules-func (plist-get evil-ts-obj-conf-raise-rules lang))
                  (rules-alist (funcall raise-rules-func 'text ))
                  (text-things (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) text-things spec)))
        (evil-ts-obj-edit--raise-operator (car range) (cadr range) count))
    (evil-ts-obj-edit--cleanup)))

;;;; Swap dwim


(defun evil-ts-obj-edit--swap-dwim (dir &optional count)
  "Swap a current text object in DIR direction.
Text objects is determined based on rules from
`evil-ts-obj-conf-swap-dwim-rules'. When COUNT is greater then 1, swap
current text object with the Nth sibling."

  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      ;; find first text object
      (when-let* ((lang (treesit-language-at (point)))
                  (swap-dwim-rules-func (plist-get evil-ts-obj-conf-swap-dwim-rules lang))
                  (rules-alist (funcall swap-dwim-rules-func 'first))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec nil t))
                  (node (caddr range)))
        (evil-ts-obj-edit--swap-operator (car range) (cadr range))
        ;; Find sibling to swap with.

        (when-let* ((first-thing (plist-get evil-ts-obj--last-text-obj-spec :thing))
                    (second-rules-alist (funcall swap-dwim-rules-func 'second))
                    (second-thing (evil-ts-obj-edit--thing-from-rules second-rules-alist))
                    (second-spec (evil-ts-obj--make-spec second-rules-alist 'op))
                    (sibling (evil-ts-obj--find-matching-sibling node first-thing dir second-thing
                                                                 count))
                    (sibling-range (evil-ts-obj--get-text-obj-range sibling second-thing second-spec)))
          (evil-ts-obj-edit--swap-operator (car sibling-range) (cadr sibling-range))
          (goto-char (car evil-ts-obj--last-text-obj-range))))
    (evil-ts-obj-edit--cleanup)))


;;;; Clone

(defun evil-ts-obj-edit--clone-dwim-impl (after? &optional comment?)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (clone-rules-func (plist-get evil-ts-obj-conf-clone-rules lang))
                  (rules-alist (funcall clone-rules-func 'text))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec nil t))
                  (node (caddr range)))
        (if after?
            (evil-ts-obj-edit--clone-after-operator (car range) (cadr range))
          (evil-ts-obj-edit--clone-before-operator (car range) (cadr range)))

        (when-let* ((second-rules-alist (funcall clone-rules-func 'place))
                    (second-thing (evil-ts-obj-edit--thing-from-rules second-rules-alist))
                    (second-spec (evil-ts-obj--make-spec second-rules-alist 'op))
                    (sibling-range (evil-ts-obj--get-text-obj-range (point) second-thing second-spec)))
          (if after?
              (evil-ts-obj-edit--clone-after-operator (car range) (cadr range) comment?)
            (evil-ts-obj-edit--clone-before-operator (car range) (cadr range) comment?))))
    (evil-ts-obj-edit--cleanup)))


;;;; Extract

(defun evil-ts-obj-edit--extract-valid-place? (place-node place-range place-things)
  "Return t if PLACE-NODE is suitable for inserting extracted text.
Check that parent of a place is compound or root node. If parent
is compound PLACE-RANGE should be inside compound inner range.
PLACE-THINGS are possible things of a place-node."

  ;; TODO put requirements of parent of a place to rules?
  (if-let* ((things (if (memq 'compound place-things)
                        place-things
                      (append place-things '(compound))))
            (parent-spec (evil-ts-obj--make-spec 'compound 'op 'inner))
            (place-parent (treesit-parent-until place-node things)))

      (when-let* ((parent-thing (evil-ts-obj--current-thing place-parent things))
                  ((eq parent-thing 'compound))
                  (place-parent-range (evil-ts-obj--get-text-obj-range
                                       place-parent 'compound parent-spec)))
        ;; check that place-range is inside parent inner range
        (and (<= (car place-parent-range) (car place-range))
             (<= (cadr place-range) (cadr place-parent-range))))
    ;; we are likely hit root node
    t))

(defun evil-ts-obj-edit--extract-operator-impl (start end &optional count after)
  "Teleport text from START END range before or AFTER parent text object.
Parent text object is determined by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (if after
      (evil-ts-obj-edit--teleport-after-operator start end)
    (evil-ts-obj-edit--teleport-before-operator start end))
  (unwind-protect
      (when-let*
          ((lang (treesit-language-at (point)))
           (extract-rules-func (plist-get evil-ts-obj-conf-extract-rules lang))
           (text-range (list start end))
           (rules-alist (funcall extract-rules-func 'place))
           (place-thing (evil-ts-obj-edit--thing-from-rules rules-alist))
           (place-spec (evil-ts-obj--make-spec rules-alist 'op))
           (place-range
            (cl-do* ((place-range
                      (evil-ts-obj--get-text-obj-range (point)
                                                       place-thing place-spec
                                                       text-range t))
                     (place-node (caddr place-range))
                     (count (or count 1))
                     (iter 1)
                     (range-valid nil))
                ((or (null place-range)
                     (and (setq range-valid
                                (evil-ts-obj-edit--extract-valid-place?
                                 place-node place-range place-thing))
                          (>= iter count)))
                 place-range)

              (if range-valid
                  (cl-incf iter)
                (setq place-range nil))

              ;; Place is not valid or iter < count, so keep searching place-node
              (setq place-node (treesit-parent-until place-node place-thing))
              (when place-node
                (setq place-range
                      (evil-ts-obj--get-text-obj-range place-node place-thing place-spec))))))

        (if after
            (evil-ts-obj-edit--teleport-after-operator (car place-range) (cadr place-range) 'safe)
          (evil-ts-obj-edit--teleport-before-operator (car place-range) (cadr place-range) 'safe))

        (when (fboundp 'evil-set-jump)
          (evil-set-jump))
        (goto-char (car evil-ts-obj--last-text-obj-range)))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--extract-dwim-impl (count &optional after)
  "Teleport current text object before or AFTER parent text object.
Current and parent text object are determined by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (extract-rules-func (plist-get evil-ts-obj-conf-extract-rules lang))
                  (rules-alist (funcall extract-rules-func 'text))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec)))
        (evil-ts-obj-edit--extract-operator-impl (car range) (cadr range) count after))
    (evil-ts-obj-edit--cleanup)))

;;;; Inject

(defun evil-ts-obj-edit--inject-handle-placeholders (place-range place-node)
  "Handle empty compound body when injecting statements into it.
Compound thing is represented with PLACE-RANGE and PLACE-NODE.
The function returns a new body range.
There are two cases:

1. If language is configured with placeholders, then if body consists
entirely from any of the placeholder, it is deleted.

2. If language is configured with brackets then brackets are inserted if
they are not found in the body yet."

  (let* ((lang (treesit-language-at (car place-range)))
         (placeholders (plist-get evil-ts-obj-conf-statement-placeholder lang))
         (brackets (plist-get evil-ts-obj-conf-compound-brackets lang))
         (range-text (buffer-substring-no-properties (car place-range) (cadr place-range)))
         (new-range place-range))

    (when (and placeholders
               (cl-some (lambda (p) (equal range-text p)) placeholders))
      (delete-region (car place-range) (cadr place-range))
      (setq new-range (list (car place-range) (car place-range))))
    (when (and brackets
               (not (evil-ts-obj-edit--find-compound-brackets brackets place-range)))
      (save-excursion
        (goto-char (treesit-node-start place-node))
        (let* ((col (current-column))
               (offs (+ 2 col))
               (indent-str (make-string col 32)))
          (goto-char (car place-range))
          (beginning-of-line)
          (newline)
          (forward-line -1)
          (insert indent-str)
          (insert (aref brackets 0))
          (goto-char (+ (cadr place-range) offs))
          (insert ?\n)
          (insert indent-str)
          (insert (aref brackets 1))

          (setq new-range (list (+ (car place-range) offs)
                                (+ (cadr place-range) offs))))))
    new-range))

(defun evil-ts-obj-edit--find-largest-thing-in-range (pos range thing)
  ""
  (let* ((cursor (treesit-node-at pos))
         (include-self t)
         node)
    (while (setq cursor (treesit-parent-until
                         cursor
                         (lambda (n)
                           (and (<= (car range) (treesit-node-start n))
                                (>= (cadr range) (treesit-node-end n))
                                (treesit-node-match-p n thing t)))
                         include-self))
      (setq node cursor
            include-self nil))
    node))

(defun evil-ts-obj-edit--next-non-comment-sibling (node dir)
  ""
  (while (and node
              (treesit-node-match-p node 'comment t))
    (setq node
          (if (eq dir 'next)
              (treesit-node-next-sibling node t)
            (treesit-node-prev-sibling node t))))
  node)

(defun evil-ts-obj-edit--inject-operator-impl (start end &optional count up?)
  "Teleport text from START END range inside next text object.
If UP? is set teleport text inside previous text object.
Next/previous text objects are determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  ;; Clean up unfinished edit operations.
  (evil-ts-obj-edit--cleanup)
  (if up?
      (evil-ts-obj-edit--teleport-after-operator start end)
    (evil-ts-obj-edit--teleport-before-operator start end))
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (inject-rules-func (plist-get evil-ts-obj-conf-inject-rules lang))
                  (text-range (list start end))
                  (text-thing (evil-ts-obj--last-thing-of-this-cmd 'statement))
                  (text-node (evil-ts-obj-edit--find-largest-thing-in-range
                              (if up? start end) text-range text-thing))
                  (dir (if up? 'prev 'next))
                  (rules-alist (funcall inject-rules-func 'place))
                  (place-spec (evil-ts-obj--make-spec rules-alist 'op))
                  (place-thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  ;; find next/prev place node
                  (place-node (evil-ts-obj--find-matching-sibling text-node text-thing
                                                                  dir place-thing))
                  (place-range (evil-ts-obj--get-text-obj-range place-node place-thing place-spec)))

        ;; dip down inside the place node count-1 times
        (let ((count (or count 1))
              (text-thing (evil-ts-obj-edit--thing-from-rules
                           (funcall inject-rules-func 'text)))
              child
              child-thing)

          (cl-dotimes (_ (1- count))
            (setq child (evil-ts-obj--thing-around (car place-range) text-thing)
                  child-thing (evil-ts-obj--current-thing child text-thing))
            (if (> (treesit-node-start child) (cadr place-range))
                (setq child nil)

              (if up?
                  (if-let ((last-sibling (evil-ts-obj--find-matching-sibling child child-thing
                                                                             'next place-thing 999)))
                      (setq child last-sibling)
                    (unless (treesit-node-match-p child place-thing t)
                      (setq child nil)))

                (when (not (treesit-node-match-p child place-thing t))
                  (setq child (evil-ts-obj--find-matching-sibling child child-thing
                                                                  'next place-thing)))))
            (if (null child)
                (cl-return place-node)
              (setq place-node child
                    place-range (evil-ts-obj--get-text-obj-range place-node place-thing place-spec)))))


        (setq place-range (evil-ts-obj-edit--inject-handle-placeholders place-range place-node))
        (if up?
            (evil-ts-obj-edit--teleport-after-operator (car place-range) (cadr place-range))
          (evil-ts-obj-edit--teleport-before-operator (car place-range) (cadr place-range)))

        (when (fboundp 'evil-set-jump)
          (evil-set-jump))
        (goto-char (car evil-ts-obj--last-text-obj-range)))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--inject-dwim-impl (count &optional up?)
  "Teleport current text object inside next text object.
If UP? is set teleport text inside previous text object.
Next/previous text objects are determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (inject-rules-func (plist-get evil-ts-obj-conf-inject-rules lang))
                  (rules-alist (funcall inject-rules-func 'text))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec)))
        (evil-ts-obj-edit--inject-operator-impl (car range) (cadr range) count up?))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj--slurp-point-position (place-node place-thing)
  (pcase-let* ((nav-spec (evil-ts-obj--make-nav-spec place-thing))
               (`(,start ,end) (evil-ts-obj--apply-modifiers place-node place-thing nav-spec t)))
    (cond
     ((= (point) start) 'beg)
     ((= (point) (1- end)) 'end)
     (t 'mid))))

(defun evil-ts-obj-edit--slurp (&optional count)
  "Extend current compound with sibling statements COUNT times.
When point is inside the compound or at the end of the compound
slurp lower statements. If point is at the beginning slurp upper
statements."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      (when-let* ((count (or count 1))
                  (lang (treesit-language-at (point)))
                  (slurp-rules-func (plist-get evil-ts-obj-conf-slurp-rules lang))
                  (place-rules-alist (funcall slurp-rules-func 'place))
                  (place-candidate-thing (evil-ts-obj-edit--thing-from-rules place-rules-alist))
                  (place-spec (evil-ts-obj--make-spec place-rules-alist 'op))
                  (place-range (evil-ts-obj--get-text-obj-range (point) place-candidate-thing
                                                                place-spec nil t))
                  (place-thing (plist-get evil-ts-obj--last-text-obj-spec :thing))
                  (place-node (caddr place-range))
                  (point-pos (evil-ts-obj--slurp-point-position place-node place-thing))
                  (text-rules-alist (funcall slurp-rules-func 'text))
                  (text-thing (evil-ts-obj-edit--thing-from-rules text-rules-alist))
                  (text-spec (evil-ts-obj--make-spec text-rules-alist 'op)))

        (let ((dir (if (eq point-pos 'beg) 'prev 'next))
              text-start-node
              text-end-node)
          (setq text-start-node
                (evil-ts-obj--find-matching-sibling place-node place-thing
                                                    dir text-thing 1)
                text-end-node
                (if (= count 1) text-start-node
                  (evil-ts-obj--find-matching-sibling place-node place-thing
                                                      dir text-thing count)))
          (when (eq point-pos 'beg)
              (cl-rotatef text-start-node text-end-node))

          (when (and text-start-node text-end-node)
            (let* ((start-range (evil-ts-obj--get-text-obj-range text-start-node text-thing text-spec))
                   (end-range (if (treesit-node-eq text-start-node text-end-node)
                                  start-range
                                (evil-ts-obj--get-text-obj-range text-end-node text-thing text-spec)))
                   (text-start (car start-range))
                   (text-end (cadr end-range)))

              ;; first teleport invocation may be after or before it does not matter.
              (evil-ts-obj-edit--teleport-after-operator text-start text-end)
              (setq place-range
                    (evil-ts-obj-edit--inject-handle-placeholders place-range place-node))
              (save-excursion
                (pcase point-pos
                  ((or 'end 'mid)
                   (evil-ts-obj-edit--teleport-after-operator (car place-range) (cadr place-range)))
                  ('beg
                   (evil-ts-obj-edit--teleport-before-operator (car place-range) (cadr place-range)))))))))

    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--barf (&optional count)
  "Shrink current compound extracting inner statements COUNT times.
When point is inside the compound or at the end of the compound
barf bottommost statements. If point is at the beginning barf
topmost statments."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      (when-let* ((count (or count 1))
                  (lang (treesit-language-at (point)))
                  (barf-rules-func (plist-get evil-ts-obj-conf-barf-rules lang))
                  (place-rules-alist (funcall barf-rules-func 'place))
                  (place-candidate-thing (evil-ts-obj-edit--thing-from-rules place-rules-alist))
                  (place-spec (evil-ts-obj--make-spec place-rules-alist 'op))
                  (place-range (evil-ts-obj--get-text-obj-range (point) place-candidate-thing
                                                                place-spec nil t))
                  (place-thing (plist-get evil-ts-obj--last-text-obj-spec :thing))
                  (place-node (caddr place-range))
                  (place-inner-range (evil-ts-obj--get-text-obj-range
                                      place-node place-thing
                                      (plist-put evil-ts-obj--last-text-obj-spec :mod 'inner)))
                  (point-pos (evil-ts-obj--slurp-point-position place-node place-thing))
                  (text-rules-alist (funcall barf-rules-func 'text))
                  (text-things (evil-ts-obj-edit--thing-from-rules text-rules-alist 'comment))
                  (text-spec (evil-ts-obj--make-spec text-rules-alist 'op)))

        (let* ((dir (if (eq point-pos 'beg) 'next 'prev))
               (start-pos (if (eq point-pos 'beg)
                              (car place-inner-range)
                            (cadr place-inner-range)))
               (text-start-node (evil-ts-obj-edit--find-largest-thing-in-range
                                 start-pos place-inner-range text-things))
               (text-start-node (evil-ts-obj-edit--next-non-comment-sibling text-start-node dir))
               start-node-thing
               text-end-node)


          (when text-start-node
            (setq start-node-thing (evil-ts-obj--current-thing text-start-node text-things))

            (setq text-end-node
                  (if (= count 1) text-start-node
                    (evil-ts-obj--find-matching-sibling text-start-node start-node-thing
                                                        dir text-things (1- count)))))

          (unless (eq point-pos 'beg)
            (cl-rotatef text-start-node text-end-node))

          (when (and text-start-node text-end-node)
            (let* ((start-range (evil-ts-obj--get-text-obj-range text-start-node text-things text-spec))
                   (end-range (if (treesit-node-eq text-start-node text-end-node)
                                  start-range
                                (evil-ts-obj--get-text-obj-range text-end-node text-things text-spec)))
                   (text-start (car start-range))
                   (text-end (cadr end-range)))

              ;; first teleport invocation may be after or before it does not matter.
              (evil-ts-obj-edit--teleport-after-operator text-start text-end)

              (pcase point-pos
                ((or 'end 'mid)
                 (evil-ts-obj-edit--teleport-after-operator (car place-range) (cadr place-range) 'safe))
                ('beg
                 (evil-ts-obj-edit--teleport-before-operator
                  (car place-range) (cadr place-range) 'safe)))))))

    (evil-ts-obj-edit--cleanup)))




(defun evil-ts-obj-edit--convolute (&optional count)
  "Swap parent node with the grandparent node for the current text node.
See `evil-ts-obj-edit--convolute-impl' for the implementation
details. When COUNT is set and is greater than 1, also select
COUNT siblings of the current text node."
  (evil-ts-obj-edit--cleanup)
  (unwind-protect
      (when-let* ((count (or count 1))
                  (lang (treesit-language-at (point)))
                  (conv-rules-func (plist-get evil-ts-obj-conf-convolute-rules lang))
                  (text-rules-alist (funcall conv-rules-func 'text))
                  (text-things (evil-ts-obj-edit--thing-from-rules text-rules-alist))
                  (text-spec (evil-ts-obj--make-spec text-rules-alist 'op))
                  (text-range (evil-ts-obj--get-text-obj-range (point) text-things text-spec nil t))
                  (text-thing (plist-get evil-ts-obj--last-text-obj-spec :thing))
                  (text-node (caddr text-range))
                  (text-range (if (> count 1)
                                  (list (car text-range)
                                        (cadr
                                         (or
                                          (evil-ts-obj--get-text-obj-range
                                           (evil-ts-obj--find-matching-sibling
                                            text-node text-thing 'next text-things (1- count))
                                           text-things text-spec)
                                          text-range)))
                                text-range))
                  (parent-rules-alist (funcall conv-rules-func 'parent))
                  (parent-candidate-thing (evil-ts-obj-edit--thing-from-rules parent-rules-alist))
                  (parent-spec (evil-ts-obj--make-spec parent-rules-alist 'op))
                  (parent-range (evil-ts-obj--get-text-obj-range (point) parent-candidate-thing
                                                                 parent-spec text-range))
                  (parent-thing (plist-get evil-ts-obj--last-text-obj-spec :thing))
                  (grandparent-range (evil-ts-obj--get-text-obj-range (point) parent-candidate-thing
                                                                      parent-spec parent-range))
                  ;; grandparent and parent should be the same thing
                  ((eq (plist-get evil-ts-obj--last-text-obj-spec :thing) parent-thing)))
        (evil-ts-obj-edit--convolute-impl text-range parent-range grandparent-range))
    (evil-ts-obj-edit--cleanup)))


(defun evil-ts-obj-edit--convolute-impl (text-range parent-range grandparent-range)
  "Swap parent node with the grandparent node for the current text node.
Each node is represented via coresponding range: TEXT-RANGE,
PARENT-RANGE, GRANDPARENT-RANGE.
It is implemented via three replace operations on provided ranges.
Initial positions:
[G [ P [ T ] P ] G ]
1. Replace text range with grandparent range.
[G [ P > [G [ P [ T ] P ] G ] < P ] G ]
2. Replace original grandparent with the parent range.
[ P [G [ P [ T ] P ] G ] P ]
3. Replace inner parent range with the text range.
[ P [G [ T ] G ] P ]"

  (put-text-property (car text-range) (cadr text-range) 'text-range t)
  (put-text-property (car parent-range) (cadr parent-range) 'parent-range t)

  (let ((end-parent-marker (copy-marker (cadr parent-range) t))
        (end-grandparent-marker (copy-marker (cadr grandparent-range) t)))
    (unwind-protect
        (progn
          (let ((text (evil-ts-obj-util--extract-text
                       (car grandparent-range) end-grandparent-marker t)))
            (remove-list-of-text-properties (car parent-range) (cadr parent-range)
                                            '(text-range parent-range))
            (evil-ts-obj-edit--simple-replace text (cons (car text-range) (cadr text-range)) t))
          (evil-ts-obj-edit--simple-replace (cons (car parent-range) end-parent-marker)
                                            (cons (car grandparent-range) end-grandparent-marker) t)

          ;; restore bounds of parent and text nodes
          (when-let* ((start-pos (max (point-min) (1- (car grandparent-range))))
                      (text-start (next-single-property-change start-pos 'text-range))
                      (text-end (next-single-property-change text-start 'text-range))
                      (parent-start (next-single-property-change start-pos 'parent-range))
                      (parent-end (next-single-property-change parent-start 'parent-range)))

            (evil-ts-obj-edit--save-range-or-call-op 'conv text-start text-end
                                                     #'evil-ts-obj-edit--replace)
            (evil-ts-obj-edit--save-range-or-call-op 'conv parent-start parent-end
                                                     #'evil-ts-obj-edit--replace)
            (goto-char (car evil-ts-obj--last-text-obj-range))))
      (remove-list-of-text-properties (car parent-range)
                                      (min (cadr parent-range) (point-max))
                                      '(text-range parent-range))
      (set-marker end-parent-marker nil)
      (set-marker end-grandparent-marker nil))))

(provide 'evil-ts-obj-edit)
;;; evil-ts-obj-edit.el ends here
