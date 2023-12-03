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


(defvar-local evil-ts-obj-conf-thing-modifiers nil
  "Plist that holds an extension function for each language in the buffer.
A function should accept two arguments: SPEC and NODE.")

(defvar-local evil-ts-obj-conf-nav-thing 'compound
  "Default thing that used in movement commands.")

(defvar-local evil-ts-obj-conf-param-sep-regexps nil
  "Plist that holds separator regexps for each language in the buffer.
Separators are generally used to create outer text object. Also
there is special behaviour when two things are separated and the
point is on a separator. We prefer the previous thing in this
case. Should be set for each language in appropriate file.")


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
            (suffix (match-string 3 sym-name)))
      (set (intern (format "evil-ts-obj-%s-%s%s-regex" lang thing suffix))
           (evil-ts-obj-conf--make-nodes-regex val))
    (user-error "Unknown pattern for a nodes symbol %s" (symbol-name sym))))


(provide 'evil-ts-obj-conf)
;;; evil-ts-obj-conf.el ends here
