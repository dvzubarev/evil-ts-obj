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
RANGE should be a cons of markers. Return the range
of the inserted content."

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
            (list init-pos (marker-position start))))))))

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
            (insert insert-text)))
      ;; just delete region
      (delete-region (car range) (cadr range)))))

(defun evil-ts-obj-edit--clone-after (text-range place-range &optional delete-range)
  "Copy content of TEXT-RANGE to the position after PLACE-RANGE.
Return new range of the cloned content. If the end of the
TARGET-RANGE is \n and content does not start with newline, then
new line is inserted and indented according to an indentation at
the beginning of the TARGET-RANGE (see
`evil-ts-obj-edit--clone-indent'). After this content of RANGE is
inserted on the new line.

Possible values of DELETE-RANGE are t, nil and safe symbol. If
DELETE-RANGE is t delete content of RANGE. If its value is safe,
then use `evil-ts-obj-edit--delete-compound-inner' function for
range deletion."

  (evil-ts-obj-edit--clone-check-ranges text-range place-range delete-range t)
  (let (text text-starts-with-newline
             text-starts-with-space
             text-first-char
             newline-after-text)
    (pcase-let ((`(,start . ,end) text-range))
      (with-current-buffer (marker-buffer start)
        (save-excursion
          (goto-char start)
          (setq text-first-char (char-after)
                text-starts-with-space (eq (char-syntax text-first-char) ?\ ))
          (skip-syntax-forward " \\")
          (setq text-starts-with-newline (eolp))
          (goto-char end)
          (setq newline-after-text (eq (char-after) ?\n)))
        (setq text (if text-starts-with-space (buffer-substring-no-properties start end)
                     (evil-ts-obj-util--extract-text start end)))))


    (pcase-let ((`(,start . ,end) place-range))
      (with-current-buffer (marker-buffer start)
        (let ((insert-pos (marker-position end))
              (place-last-char (char-after (1- end))))
          (save-excursion
            (goto-char insert-pos)
            (cond
             ((and (eq (char-after insert-pos) ?\n)
                   ;; heuristics to determine whether to open a new line
                   (not text-starts-with-newline)
                   (or
                    (eq (char-syntax place-last-char) ?.)
                    (not (evil-ts-obj-edit--clone-compatible? place-last-char text-first-char))))
              ;; insert new line and insert on it.
              ;; Use indentation of the start position.
              (newline)
              (evil-ts-obj-edit--clone-indent start)
              (end-of-line)
              (setq insert-pos (point)))
             ((not (evil-ts-obj-edit--clone-compatible? place-last-char text-first-char))
              (insert " ")
              (setq insert-pos (point)))
             ((not (evil-ts-obj-edit--clone-compatible? (aref text (1- (length text)))
                                                        (char-after end)))
              ;; need to insert a space before the text that is next to insert point
              (save-excursion
                (if (not newline-after-text)
                    (insert " ")
                  (insert (concat "\n" (make-string (current-column) 32)))))))

            (let ((indented-text (if (not text-starts-with-space)
                                     (evil-ts-obj-util--indent-text-according-to-point-pos text)
                                   text)))
              (insert indented-text)
              ;; set inserted text range to place-range
              (setf (car place-range) (set-marker start insert-pos)
                    (cdr place-range) (set-marker end (+ insert-pos (length indented-text)))))))))

    (pcase-let ((`(,start . ,end) text-range))
      (with-current-buffer (marker-buffer start)
        (pcase delete-range
          ('safe (evil-ts-obj-edit--delete-compound-inner text-range))
          ('t (delete-region start end)))))
    (list (marker-position (car place-range))
          (marker-position (cdr place-range)))))

(defun evil-ts-obj-edit--clone-before (text-range place-range &optional delete-range)
  "Copy content of TEXT-RANGE before the beginning of PLACE-RANGE.
Return new range of the cloned content. If beginning of the
TARGET-RANGE preceded by only spaces and new content does not end
with newline, then a new line is inserted and indented according
to an indentation at the beginning of the TARGET-RANGE (see
`evil-ts-obj-edit--clone-indent'). After this content of RANGE is
inserted on the new line.

Possible values of DELETE-RANGE are t, nil and safe symbol. If
DELETE-RANGE is t delete content of RANGE. If its value is safe,
then use `evil-ts-obj-edit--delete-compound-inner' function for
range deletion."
  (evil-ts-obj-edit--clone-check-ranges text-range place-range delete-range nil)
  (let (text text-starts-with-space
             text-ends-with-newline
             text-last-char
             newline-after-text)
    (pcase-let ((`(,start . ,end) text-range))
      (with-current-buffer (marker-buffer start)
        (save-excursion
          (goto-char end)
          (setq text-last-char (char-after (1- (point)))
                newline-after-text (eq (char-after) ?\n))
          (skip-chars-backward " \t")
          (setq text-ends-with-newline (bolp))
          (goto-char start)
          (setq text-starts-with-space (eq (char-syntax (char-after)) ?\ )))
        (setq text (if text-starts-with-space
                       (buffer-substring-no-properties start end)
                     (evil-ts-obj-util--extract-text start end)))
        (pcase delete-range
          ('safe (evil-ts-obj-edit--delete-compound-inner text-range))
          ('t (delete-region start end)))))

    (pcase-let ((`(,start . ,end) place-range))
      (with-current-buffer (marker-buffer start)
        (let ((insert-pos (marker-position start))
              (point-mark (copy-marker (max start (point)) t))
              indent)
          (goto-char insert-pos)
          (cond
           ;; heuristics to determine whether to open a new line
           ((and (not (eolp))
                 (save-excursion (skip-chars-backward " \t") (bolp))
                 (not text-ends-with-newline))

            ;; Insert point is at the start of the line.
            ;; Insert new line with the indentation of the start position.
            (goto-char (line-beginning-position))
            (newline)
            (forward-line -1)
            (evil-ts-obj-edit--clone-indent start)
            (end-of-line)
            (setq insert-pos (point)))
           ((and
             newline-after-text
             (or (eq (char-syntax text-last-char) ?.)
                 (not (evil-ts-obj-edit--clone-compatible? text-last-char (char-after start)))))
            ;; Move current content down one line
            (setq indent (current-column))
            (open-line 1)
            (setq insert-pos (point))
            (forward-line 1)
            (set-marker-insertion-type start nil)
            (indent-to indent)
            (evil-ts-obj-edit--clone-indent insert-pos start end)
            (goto-char insert-pos))
           ((not (evil-ts-obj-edit--clone-compatible? text-last-char (char-after start)))
            (save-excursion
              (insert " "))))

          (let ((indented-text (if (not text-starts-with-space)
                                   (evil-ts-obj-util--indent-text-according-to-point-pos text)
                                 text)))
            (insert indented-text)
            (goto-char point-mark)
            (set-marker point-mark nil)
            (list insert-pos (+ insert-pos (length indented-text)))))))))


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
            (not (buffer-live-p (marker-buffer (cadr evil-ts-obj-edit--saved-range)))))
        ;; first call with this op-type
        (progn
          (evil-ts-obj-edit--cleanup)
          (setq evil-ts-obj-edit--saved-range (cons op-type (cons start-marker end-marker)))
          (evil-ts-obj-edit--highlight-range start end))
      ;; second call
      (let ((second-markers (cons start-marker end-marker)))
        (unwind-protect
            (setq evil-ts-obj--last-text-obj-range
                  (funcall op-func
                           (cdr evil-ts-obj-edit--saved-range)
                           second-markers))
          (evil-ts-obj-edit--cleanup second-markers))))))

;;; Operators

(defun evil-ts-obj-edit--replace-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'replace start end #'evil-ts-obj-edit--replace))

(defun evil-ts-obj-edit--swap-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'swap start end #'evil-ts-obj-edit--swap))

(defun evil-ts-obj-edit--clone-after-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'clone start end #'evil-ts-obj-edit--clone-after))

(defun evil-ts-obj-edit--teleport-after-operator (start end &optional del-type)
  (evil-ts-obj-edit--save-range-or-call-op
   'teleport start end
   (lambda (r or) (evil-ts-obj-edit--clone-after r or (or del-type t)))))

(defun evil-ts-obj-edit--clone-before-operator (start end)
  (evil-ts-obj-edit--save-range-or-call-op 'clone start end #'evil-ts-obj-edit--clone-before))

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
                                                          last-range t)))
        (when-let* ((count (or count 1))
                    ((> count 1))
                    (node (caddr range))
                    (parent node))
          (cl-dotimes (_ (1- count))
            (if (setq parent (treesit-parent-until
                              node (lambda (n) (treesit-node-match-p n parent-thing t))))
                (setq node parent)
              (cl-return node)))
          (setq range (evil-ts-obj--get-text-obj-range node parent-thing spec)))

        (evil-ts-obj-edit--save-range-or-call-op 'raise (car range) (cadr range)
                                                 #'evil-ts-obj-edit--replace))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--raise-dwim (&optional count)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (raise-rules-func (plist-get evil-ts-obj-conf-raise-rules lang))
                  (rules-alist (funcall raise-rules-func 'text ))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec)))
        (evil-ts-obj-edit--raise-operator (car range) (cadr range) count))
    (evil-ts-obj-edit--cleanup)))

;;;; Drag

(defun evil-ts-obj-edit--drag (dir &optional count)
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
                    (sibling (evil-ts-obj--find-matching-sibling node first-thing dir second-thing
                                                                 count))
                    (sibling-range (evil-ts-obj--get-text-obj-range sibling second-thing second-spec)))
          (evil-ts-obj-edit--swap-operator (car sibling-range) (cadr sibling-range))
          (goto-char (car evil-ts-obj--last-text-obj-range))))
    (evil-ts-obj-edit--cleanup)))


;;;; Clone

(defun evil-ts-obj-edit--clone-dwim-impl (after?)
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

        (when-let* ((last-spec evil-ts-obj--last-text-obj-spec)
                    (first-thing (plist-get last-spec :thing))
                    (second-rules-alist (funcall clone-rules-func 'place last-spec))
                    (second-thing (evil-ts-obj-edit--thing-from-rules second-rules-alist))
                    (second-spec (evil-ts-obj--make-spec second-rules-alist 'op))
                    (sibling-range (evil-ts-obj--get-text-obj-range (point) second-thing second-spec)))
          (if after?
              (evil-ts-obj-edit--clone-after-operator (car range) (cadr range))
            (evil-ts-obj-edit--clone-before-operator (car range) (cadr range)))))
    (evil-ts-obj-edit--cleanup)))


;;;; Extract

(defun evil-ts-obj-edit--extract-operator-impl (start end &optional count after)
  "Teleport text from START END range before or AFTER parent thing.
Parent thing is determined by the cdr of `evil-ts-obj-conf-extract-rules'.
When COUNT is set select Nth parent."
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (if after
      (evil-ts-obj-edit--teleport-after-operator start end)
    (evil-ts-obj-edit--teleport-before-operator start end))
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (extract-rules-func (plist-get evil-ts-obj-conf-extract-rules lang))
                  (last-spec evil-ts-obj--last-text-obj-spec)
                  (last-range evil-ts-obj--last-text-obj-range)
                  (rules-alist (funcall extract-rules-func 'place last-spec))
                  (place-thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point)
                                                          place-thing spec
                                                          last-range t)))
        (when-let* ((count (or count 1))
                    ((> count 1))
                    (node (caddr range))
                    (parent node))
          (cl-dotimes (_ (1- count))
            (if (setq parent (treesit-parent-until
                              node (lambda (n) (treesit-node-match-p n place-thing t))))
                (setq node parent)
              (cl-return node)))
          (setq range (evil-ts-obj--get-text-obj-range node place-thing spec)))

        (if after
            (evil-ts-obj-edit--teleport-after-operator (car range) (cadr range) 'safe)
          (evil-ts-obj-edit--teleport-before-operator (car range) (cadr range) 'safe))

        (when (fboundp 'evil-set-jump)
          (evil-set-jump))
        (goto-char (car evil-ts-obj--last-text-obj-range)))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--extract-dwim-impl (count &optional after)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (extract-rules-func (plist-get evil-ts-obj-conf-extract-rules lang))
                  (rules-alist (funcall extract-rules-func 'text ))
                  (thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (spec (evil-ts-obj--make-spec rules-alist 'op))
                  (range (evil-ts-obj--get-text-obj-range (point) thing spec)))
        (evil-ts-obj-edit--extract-operator-impl (car range) (cadr range) count after))
    (evil-ts-obj-edit--cleanup)))

;;;; Inject

(defun evil-ts-obj-edit--inject-handle-placeholders (place-range place-node)
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

(defun evil-ts-obj-edit--inject-operator-impl (start end &optional count up?)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)
  (if up?
      (evil-ts-obj-edit--teleport-after-operator start end)
    (evil-ts-obj-edit--teleport-before-operator start end))
  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (inject-rules-func (plist-get evil-ts-obj-conf-inject-rules lang))
                  (last-spec evil-ts-obj--last-text-obj-spec)
                  (last-range evil-ts-obj--last-text-obj-range)
                  (rules-alist (funcall inject-rules-func 'place last-spec))
                  (place-spec (evil-ts-obj--make-spec rules-alist 'op))
                  (place-thing (evil-ts-obj-edit--thing-from-rules rules-alist))
                  (dir (if up? 'prev 'next))
                  (last-thing (plist-get last-spec :thing))
                  (last-node (evil-ts-obj--thing-around (point) last-thing))
                  ;; find next/prev place node
                  (place-node (evil-ts-obj--find-matching-sibling last-node last-thing
                                                                  dir place-thing))
                  (range (evil-ts-obj--get-text-obj-range place-node place-thing place-spec)))

        ;; dip down inside the place node count-1 times
        (let ((count (or count 1))
              (text-thing (evil-ts-obj-edit--thing-from-rules
                           (funcall inject-rules-func 'text)))
              child
              child-thing)

          (cl-dotimes (_ (1- count))
            (setq child (evil-ts-obj--thing-around (car range) text-thing)
                  child-thing (evil-ts-obj--current-thing child text-thing))
            (if (> (treesit-node-start child) (cadr range))
                (setq child nil)

              (if up?
                  (let (sibling)
                    (setq sibling (evil-ts-obj--find-matching-sibling child child-thing
                                                                      'next place-thing 999))
                    (if sibling
                        (setq child sibling)
                      (unless (treesit-node-match-p child place-thing t)
                        (setq child nil))))

                (when (not (treesit-node-match-p child place-thing t))
                  (setq child (evil-ts-obj--find-matching-sibling child child-thing
                                                                  'next place-thing)))))
            (if (null child)
                (cl-return place-node)
              (setq place-node child
                    range (evil-ts-obj--get-text-obj-range place-node place-thing place-spec)))))


        (setq range (evil-ts-obj-edit--inject-handle-placeholders range place-node))
        (if up?
            (evil-ts-obj-edit--teleport-after-operator (car range) (cadr range))
          (evil-ts-obj-edit--teleport-before-operator (car range) (cadr range)))

        (when (fboundp 'evil-set-jump)
          (evil-set-jump))
        (goto-char (car evil-ts-obj--last-text-obj-range)))
    (evil-ts-obj-edit--cleanup)))

(defun evil-ts-obj-edit--inject-dwim-impl (count &optional up?)
  ;; clean unfinished edit operations
  (evil-ts-obj-edit--cleanup)

  (unwind-protect
      (when-let* ((lang (treesit-language-at (point)))
                  (inject-rules-func (plist-get evil-ts-obj-conf-inject-rules lang))
                  (rules-alist (funcall inject-rules-func 'text ))
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
When inside the compound or at the end of compound slurp on the right.
If point is at the beginning slurp to the left."
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


(provide 'evil-ts-obj-edit)
;;; evil-ts-obj-edit.el ends here
