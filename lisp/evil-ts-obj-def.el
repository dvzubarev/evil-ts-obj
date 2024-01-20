;;; evil-ts-obj-def.el --- Description -*- lexical-binding: t; -*-
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
;;  Default functions
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'evil-ts-obj-core)


;;; Default functions
;;;; Edit rules

;;;;; raise rules
(defun evil-ts-obj-def-conf-lang-raise-rules (range-type &optional text-spec)
  "Return default raise rules for configuration languages like YAML.
See `evil-ts-obj-conf-raise-rules' for description of
RANGE-TYPE and TEXT-SPEC."
  (pcase range-type
    ('text
     '((param . inner)
       (compound . outer)))
    ('place
     (pcase text-spec
       ((pmap (:thing 'compound))
        '((param . inner)
          (compound . outer)))
       ((pmap (:thing 'param))
        '((param . inner)
          (compound . inner)))))))

(defun evil-ts-obj-def-raise-rules (range-type &optional text-spec)
  "Return default raise rules.
See `evil-ts-obj-conf-raise-rules' for description of
RANGE-TYPE and TEXT-SPEC."
  (pcase range-type
    ('text
     '((param . inner)
       (statement . inner)
       (compound . outer)))
    ('place
     (pcase text-spec
       ((pmap (:thing (or 'compound 'statement)))
        '((compound . outer)
          (statement . inner)))
       ((pmap (:thing 'param))
        '((statement . inner)
          (param . all)))))))

;;;;; drag rules

(defun evil-ts-obj-def-drag-rules (_range-type &optional _first-spec)
  "Return default drag rules.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE
and FIRST-SPEC."
  '((param . inner)
    (statement . inner)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-drag-rules (_range-type &optional _text-spec)
  "Return default drag rules for configuration languages like YAML.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE
and TEXT-SPEC."
  '((param . inner)
    (compound . outer)))

;;;;; clone rules

(defun evil-ts-obj-def-clone-rules (_range-type &optional _text-spec)
  "Return default clone rules.
See `evil-ts-obj-conf-clone-rules' for description of RANGE-TYPE
and FIRST-SPEC."
  '((param . outer)
    (statement . outer)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-clone-rules (_range-type &optional _text-spec)
  "Return default clone rules for configuration languages like YAML.
See `evil-ts-obj-conf-clone-rules' for description of RANGE-TYPE
and FIRST-SPEC."
  '((param . outer)
    (compound . outer)))

;;;;; extract rules

(defun evil-ts-obj-def-extract-rules (_range-type &optional _text-spec)
  "Return default clone rules.
See `evil-ts-obj-conf-extract-rules' for description of RANGE-TYPE
and FIRST-SPEC."
  '((statement . inner)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-extract-rules (range-type &optional _text-spec)
  "Return default clone rules for configuration languages like YAML.
See `evil-ts-obj-conf-extract-rules' for description of RANGE-TYPE
and FIRST-SPEC."

  (pcase range-type
    ('text
     '((param . inner)
       (compound . outer)))
    ('place
     '((param . inner)))))


;;;; sibling traverse helpers

(defvar evil-ts-obj-def--sibling-trav (evil-ts-obj-trav-create
                                       :fetcher #'evil-ts-obj--get-sibling-simple
                                       :kind-func #'evil-ts-obj--get-node-kind)
  "Default value for sibling traversing.")

(defun evil-ts-obj-def--create-sibling-trav (user-trav)
  "Create sibling traversing object combining USER-TRAV with the default one."
  (evil-ts-obj-trav-create
   :seps (evil-ts-obj-trav-seps user-trav)
   :fetcher (or (evil-ts-obj-trav-fetcher user-trav)
                (evil-ts-obj-trav-fetcher evil-ts-obj-def--sibling-trav))
   :kind-func (or (evil-ts-obj-trav-kind-func user-trav)
                  (evil-ts-obj-trav-kind-func evil-ts-obj-def--sibling-trav))))


;;;; init functions
(cl-defun evil-ts-obj-def-init-lang (
                                     lang things &optional &key
                                     (ext-func nil)
                                     (seps-reg nil)
                                     (nav-thing '(or param statement compound))
                                     (compound-sib-trav evil-ts-obj-def--sibling-trav)
                                     (statement-sib-trav evil-ts-obj-def--sibling-trav)
                                     (param-sib-trav evil-ts-obj-def--sibling-trav)
                                     (clone-indent-policy 'cur-indent)
                                     (compound-brackets nil)
                                     (statement-placeholder nil))
  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

SEPS-REG is regexp for language separators, it is added to
`evil-ts-obj-conf-sep-regexps'. If SEPS-REG is a list, then it is
converted to regexp via `evil-ts-obj-conf--make-nodes-regex'.

NAV-THING - added to `evil-ts-obj-conf-nav-things', default
value: (or param statement compound).

COMPOUND-SIB-TRAV, STATEMENT-SIB-TRAV, PARAM-SIB-TRAV define
sibling traversing. They are objects of type `evil-ts-obj-trav'.
An object can be instantiated using `evil-ts-obj-trav-create'. It
accepts following arguments: :seps - regexp for text object
separators (default nil), :fetcher - a function for fetching
next/previous sibling (default
`evil-ts-obj--get-sibling-simple'), :kind-func - a function for
labeling a fetched node (default `evil-ts-obj--get-node-kind').
It is possible to leave some slots undefined. In that case the
default values will be used. An object will be added to
`evil-ts-obj-conf-sibling-trav' variable, see description of this
variable for more information about functions.

This function also adds `evil-ts-obj--finalize-text-obj-range' to
`evil-ts-obj-conf-range-finalizers',
`evil-ts-obj-def-raise-rules' to `evil-ts-obj-conf-raise-rules'."


  (make-local-variable 'treesit-thing-settings)
  (cl-callf append (alist-get lang treesit-thing-settings) things)

  (when ext-func
    (cl-callf plist-put evil-ts-obj-conf-thing-modifiers lang ext-func))

  (when seps-reg
    (when (listp seps-reg)
      (setq seps-reg (evil-ts-obj-conf--make-nodes-regex seps-reg)))
    (cl-callf plist-put evil-ts-obj-conf-sep-regexps lang seps-reg))

  (cl-callf plist-put evil-ts-obj-conf-nav-things lang nav-thing)

  (let (sibl-trav-plist)
    (when compound-sib-trav
      (cl-callf plist-put sibl-trav-plist 'compound
                (evil-ts-obj-def--create-sibling-trav compound-sib-trav)))
    (when statement-sib-trav
      (cl-callf plist-put sibl-trav-plist 'statement
                (evil-ts-obj-def--create-sibling-trav statement-sib-trav)))
    (when param-sib-trav
      (cl-callf plist-put sibl-trav-plist 'param
                (evil-ts-obj-def--create-sibling-trav param-sib-trav)))
    (cl-callf plist-put evil-ts-obj-conf-sibling-trav lang sibl-trav-plist))


  (cl-callf plist-put evil-ts-obj-conf-range-finalizers lang #'evil-ts-obj--finalize-text-obj-range)
  (cl-callf plist-put evil-ts-obj-conf-raise-rules lang #'evil-ts-obj-def-raise-rules)
  (cl-callf plist-put evil-ts-obj-conf-drag-rules lang #'evil-ts-obj-def-drag-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-rules lang #'evil-ts-obj-def-clone-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-indent-policy lang clone-indent-policy)

  (cl-callf plist-put evil-ts-obj-conf-extract-rules lang #'evil-ts-obj-def-extract-rules)
  (cl-callf plist-put evil-ts-obj-conf-compound-brackets lang compound-brackets)
  (cl-callf plist-put evil-ts-obj-conf-statement-placeholder lang statement-placeholder))


(cl-defun evil-ts-obj-def-init-conf-lang (
                                          lang things &optional &key
                                          (ext-func nil)
                                          (seps-reg nil)
                                          (nav-thing '(or param compound))
                                          (compound-sib-trav evil-ts-obj-def--sibling-trav)
                                          (param-sib-trav evil-ts-obj-def--sibling-trav)
                                          (clone-indent-policy nil))

  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

SEPS-REG is regexp for language separators, it is added to
`evil-ts-obj-conf-sep-regexps'. If SEPS-REG is a list, then it is
converted to regexp via `evil-ts-obj-conf--make-nodes-regex'.

NAV-THING - added to `evil-ts-obj-conf-nav-things', default
value: (or param compound).

For information about COMPOUND-SIB-TRAV, PARAM-SIB-TRAV see
`evil-ts-obj-def-init-lang'.

This function also adds `evil-ts-obj-def-raise-rules' to
`evil-ts-obj-def-conf-lang-raise-rules'."

  (make-local-variable 'treesit-thing-settings)
  (cl-callf append (alist-get lang treesit-thing-settings) things)

  (when ext-func
    (cl-callf plist-put evil-ts-obj-conf-thing-modifiers lang ext-func))

  (when seps-reg
    (when (listp seps-reg)
      (setq seps-reg (evil-ts-obj-conf--make-nodes-regex seps-reg)))
    (cl-callf plist-put evil-ts-obj-conf-sep-regexps lang seps-reg))

  (let (sibl-trav-plist)
    (when compound-sib-trav
      (cl-callf plist-put sibl-trav-plist 'compound
                (evil-ts-obj-def--create-sibling-trav compound-sib-trav)))
    (when param-sib-trav
      (cl-callf plist-put sibl-trav-plist 'param
                (evil-ts-obj-def--create-sibling-trav param-sib-trav)))
    (cl-callf plist-put evil-ts-obj-conf-sibling-trav lang sibl-trav-plist))



  (cl-callf plist-put evil-ts-obj-conf-nav-things lang nav-thing)

  (cl-callf plist-put evil-ts-obj-conf-raise-rules
    lang #'evil-ts-obj-def-conf-lang-raise-rules)
  (cl-callf plist-put evil-ts-obj-conf-drag-rules
    lang #'evil-ts-obj-def-conf-lang-drag-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-rules
    lang #'evil-ts-obj-def-conf-lang-clone-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-indent-policy lang clone-indent-policy)
  (cl-callf plist-put evil-ts-obj-conf-extract-rules lang
            #'evil-ts-obj-def-conf-lang-extract-rules))


(provide 'evil-ts-obj-def)
;;; evil-ts-obj-def.el ends here
