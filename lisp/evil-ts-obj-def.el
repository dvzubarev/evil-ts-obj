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
        '((param . all)
          (compound . outer)))))))

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

(defun evil-ts-obj-def-drag-rules (_range-type &optional _text-spec)
  "Return default drag rules.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE
and TEXT-SPEC."
  '((param . inner)
    (statement . inner)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-drag-rules (_range-type &optional _text-spec)
  "Return default drag rules.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE
and TEXT-SPEC."
  '((param . inner)
    (compound . outer)))



;;;; init functions
(cl-defun evil-ts-obj-def-init-lang (
                                     lang things &optional &key
                                     (ext-func nil)
                                     (seps-reg nil)
                                     (nav-thing '(or param statement compound))
                                     (stmnt-add-sibl-rules t)
                                     (stmnt-seps-reg nil)
                                     (stmnt-sibl-fetcher #'evil-ts-obj--get-sibling-simple)
                                     (stmnt-sibl-kind #'evil-ts-obj--get-node-kind)
                                     (param-add-sibl-rules t)
                                     (param-seps-reg nil)
                                     (param-sibl-fetcher #'evil-ts-obj--get-sibling-simple)
                                     (param-sibl-kind #'evil-ts-obj--get-node-kind))
  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

SEPS-REG is regexp for language separators, it is added to
`evil-ts-obj-conf-sep-regexps'. If SEPS-REG is a list, then it is
converted to regexp via `evil-ts-obj-conf--make-nodes-regex'.

NAV-THING - added to `evil-ts-obj-conf-nav-things', default
value: (or param statement compound).

STMNT-ADD-SIBL-RULES add rules for traversing statement siblings,
true by default. STMNT-SEPS-REG (default nil),
STMNT-SIBL-FETCHER (default `evil-ts-obj--get-sibling-simple'),
STMNT-SIBL-KIND (default `evil-ts-obj--get-node-kind') will be
used to create `evil-ts-obj-trav' struct object. Created object
will be added to `evil-ts-obj-conf-sibling-trav' variable.
Analogues keywords for the param thing: PARAM-ADD-SIBL-RULES,
PARAM-SEPS-REG, PARAM-SIBL-FETCHER, PARAM-SIBL-KIND.

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
    (when stmnt-add-sibl-rules
      (cl-callf plist-put sibl-trav-plist 'statement
                (evil-ts-obj-trav-create
                 :seps stmnt-seps-reg
                 :fetcher stmnt-sibl-fetcher
                 :kind-func stmnt-sibl-kind)))
    (when param-add-sibl-rules
      (cl-callf plist-put sibl-trav-plist 'param
                (evil-ts-obj-trav-create
                 :seps param-seps-reg
                 :fetcher param-sibl-fetcher
                 :kind-func param-sibl-kind)))
    (cl-callf plist-put evil-ts-obj-conf-sibling-trav lang sibl-trav-plist))


  (cl-callf plist-put evil-ts-obj-conf-range-finalizers lang #'evil-ts-obj--finalize-text-obj-range)
  (cl-callf plist-put evil-ts-obj-conf-raise-rules lang #'evil-ts-obj-def-raise-rules)
  (cl-callf plist-put evil-ts-obj-conf-drag-rules lang #'evil-ts-obj-def-drag-rules))

(cl-defun evil-ts-obj-def-init-conf-lang (
                                          lang things &optional &key
                                          (ext-func nil)
                                          (seps-reg nil)
                                          (nav-thing '(or param compound))
                                          (param-add-sibl-rules t)
                                          (param-seps-reg nil)
                                          (param-sibl-fetcher #'evil-ts-obj--get-sibling-simple)
                                          (param-sibl-kind #'evil-ts-obj--get-node-kind))
  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

SEPS-REG is regexp for language separators, it is added to
`evil-ts-obj-conf-sep-regexps'. If SEPS-REG is a list, then it is
converted to regexp via `evil-ts-obj-conf--make-nodes-regex'.

NAV-THING - added to `evil-ts-obj-conf-nav-things', default
value: (or param compound).

PARAM-ADD-SIBL-RULES add rules for traversing statement siblings,
true by default. PARAM-SEPS-REG (default nil),
PARAM-SIBL-FETCHER (default `evil-ts-obj--get-sibling-simple'),
PARAM-SIBL-KIND (default `evil-ts-obj--get-node-kind') will be
used to create `evil-ts-obj-trav' struct object. Created object
will be added to `evil-ts-obj-conf-sibling-trav' variable.

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
    (when param-add-sibl-rules
      (cl-callf plist-put sibl-trav-plist 'param
                (evil-ts-obj-trav-create
                 :seps param-seps-reg
                 :fetcher param-sibl-fetcher
                 :kind-func param-sibl-kind)))
    (cl-callf plist-put evil-ts-obj-conf-sibling-trav lang sibl-trav-plist))



  (cl-callf plist-put evil-ts-obj-conf-nav-things lang nav-thing)

  (cl-callf plist-put evil-ts-obj-conf-raise-rules
    lang #'evil-ts-obj-def-conf-lang-raise-rules)
  (cl-callf plist-put evil-ts-obj-conf-drag-rules
    lang #'evil-ts-obj-def-conf-lang-drag-rules))


(provide 'evil-ts-obj-def)
;;; evil-ts-obj-def.el ends here
