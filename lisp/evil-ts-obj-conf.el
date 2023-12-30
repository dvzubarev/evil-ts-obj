;;; evil-ts-obj-conf.el --- Conf utils -*- lexical-binding: t; -*-
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

;; * Customs
(defgroup evil-ts-obj nil
  "Provide evil text-objects using tree-sitter."
  :group 'tools)

(defcustom evil-ts-obj-compound-text-obj-key "e"
  "Default key binding for compound text objects."
  :type 'string)

(defcustom evil-ts-obj-statement-text-obj-key "s"
  "Default key binding for statement text objects."
  :type 'string)

(defcustom evil-ts-obj-param-text-obj-key "a"
  "Default key binding for param text objects."
  :type 'string)

(defcustom evil-ts-obj-navigation-keys-prefix
  '((beginning-of . "(")
    (end-of . ")")
    (previous . "[")
    (next . "]")
    (previous-largest . "{")
    (next-largest . "}"))
  "Default bindings for movement commands."
  :type 'alist)


;; * Variables

(defvar-local evil-ts-obj-conf-thing-modifiers nil
  "Plist that holds an extension function for each language in the buffer.
A function returns a range (a list of two integers) of a text
object or nil. If nil is returned than default range is
calculated (see `evil-ts-obj--default-range'). The function
should accept two arguments: SPEC and NODE.

SPEC is a plist that contains a context for a current text
object. The possible fields are :thing, :op-kind, :text-obj,
:command and :visual. :thing should contain the current thing that is
represented by the treesit NODE. :text-obj may contain current
text object (inner/outer etc.). It also may be nil if text object
is not needed for the command. For example, movement commands
work on the level of things for now, therefore they do not
specify text objects in SPEC plist. :op-kind is a type of a
current operation with the text object. :op-kind can be one of
the following symbols:

* mod - any evil operator or other modification operations like
  avy actions.

* nav - any movement command like evil-ts-obj-next-thing. Also it
is used when selecting a text object for preview (e.g. when
collecting avy candidates)

:command contains that current command that will operate on
returned range. :visual is set to t if `evil-visual-state-p'
returns t, when spec is created. Also see
`evil-ts-obj--make-spec' and `evil-ts-obj--apply-modifiers'.")

(defvar-local evil-ts-obj-conf-nav-things nil
  "This plist defines default things for movement for each language.
It can be single thing, e.g. compound or a list of things: \(or
param compound thing\). When list is provided navigating command
will search for the each thing from this list until matching
thing is found.")

(defvar-local evil-ts-obj-conf-sep-regexps nil
  "Plist that holds separator regexps for each language in the buffer.
Some outer text objects may extend to the nearest separator.
Special handling of separators is needed since they usually are
siblings to the node that represent a thing. Also there is
special behavior when two things are separated and the point is
on a separator. We prefer the previous thing in this case. Should
be set for each language in appropriate file.")

;; * Helper functions

(defun evil-ts-obj-conf--make-nodes-regex (nodes)
  "Create regex from NODES."
  (format "^%s$" (regexp-opt nodes)))

(defun evil-ts-obj-conf-nodes-setter (sym val)
  "Setter for use with `evil-ts-obj-<lang>-<thing>[suffix]?-nodes` defcustoms.
It sets SYM to VAL and updates variable that holds regex built
from node names."
  (set-default sym val)
  (if-let* ((sym-name (symbol-name sym))
            ((string-match "evil-ts-obj-\\([a-z]+\\)-\\([a-z]+\\)\\([-a-z]*\\)-nodes" sym-name))
            (lang (match-string 1 sym-name))
            (thing (match-string 2 sym-name))
            (suffix (match-string 3 sym-name))
            (sym (intern (format "evil-ts-obj-%s-%s%s-regex" lang thing suffix))))
      (if (boundp sym)
          (set sym (evil-ts-obj-conf--make-nodes-regex val))
        (user-error "Variable %s is not defined!" sym))
    (user-error "Unknown pattern for a nodes symbol %s" (symbol-name sym))))

(defun evil-ts-obj-conf-seps-setter (sym val)
  "Setter for use with `evil-ts-obj-<lang>-<thing>-seps` defcustoms.
It sets SYM to VAL and updates variable that holds regex built
from node names. It also updates the variable
evil-ts-obj-<lang>-all-seps-regex if it is bound."
  (set-default sym val)
  (if-let* ((sym-name (symbol-name sym))
            ((string-match "evil-ts-obj-\\([a-z]+\\)-\\([a-z]+\\)-seps" sym-name))
            (lang (match-string 1 sym-name))
            (thing (match-string 2 sym-name))
            (sym (intern (format "evil-ts-obj-%s-%s-seps-regex" lang thing)))
            (all-sym (intern (format "evil-ts-obj-%s-all-seps-regex" lang)))
            (val (or (and (listp val) val)
                     (list val))))
      (progn
        (if (boundp sym)
            (set sym (evil-ts-obj-conf--make-nodes-regex val))
          (user-error "Variable %s is not defined!" sym))
        (when (boundp all-sym)
          (let ((all-seps nil))
            (dolist (thing '(statement param))
              (when-let* ((sym-other (intern (format "evil-ts-obj-%s-%s-seps" lang thing)))
                         ((boundp sym-other))
                         (val (ensure-list (symbol-value sym-other))))
                (setq all-seps (append all-seps val))))
            (set all-sym (evil-ts-obj-conf--make-nodes-regex all-seps)))))

    (user-error "Unknown pattern for a nodes symbol %s" (symbol-name sym))))


(provide 'evil-ts-obj-conf)
;;; evil-ts-obj-conf.el ends here
