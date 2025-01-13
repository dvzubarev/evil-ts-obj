;;; evil-ts-obj-def.el --- Description -*- lexical-binding: t; -*-
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
;;  Default functions
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'evil-ts-obj-core)


;;; Default functions
;;;; Edit rules

;;;;; raise rules
(defun evil-ts-obj-def-conf-lang-raise-rules (range-type)
  "Return default raise rules for configuration languages like YAML.
See `evil-ts-obj-conf-raise-rules' for description of
RANGE-TYPE and TEXT-THING."
  (pcase range-type
    ('text
     '((param . inner)
       (compound . outer)))
    ('place
     (pcase (evil-ts-obj--last-thing-of-this-cmd)
       ((or 'compound 'opaque)
        '((param . inner)
          (compound . outer)))
       ('param
        '((param . inner)
          (compound . inner)))))))

(defun evil-ts-obj-def-raise-rules (range-type)
  "Return default raise rules.
See `evil-ts-obj-conf-raise-rules' for description of
RANGE-TYPE."
  (pcase range-type
    ('text
     '((param . inner)
       (statement . inner)
       (compound . outer)))
    ('place
     (pcase (evil-ts-obj--last-thing-of-this-cmd)
       ((or 'compound 'statement 'opaque)
        '((compound . outer)
          (statement . inner)))
       ('param
        '((statement . inner)
          (param . all)))))))

;;;;; drag rules

(defun evil-ts-obj-def-drag-rules (_range-type)
  "Return default drag rules.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE."
  '((param . inner)
    (statement . inner)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-drag-rules (_range-type)
  "Return default drag rules for configuration languages like YAML.
See `evil-ts-obj-conf-drag-rules' for description of RANGE-TYPE."
  '((param . inner)
    (compound . outer)))

;;;;; swap-dwim rules

(defun evil-ts-obj-def-swap-dwim-rules (_range-type)
  "Return default swap-dwim rules.
See `evil-ts-obj-conf-swap-dwim-rules' for description of RANGE-TYPE."
  '((param . inner)
    (statement . inner)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-swap-dwim-rules (_range-type)
  "Return default swap-dwim rules for configuration languages like YAML.
See `evil-ts-obj-conf-swap-dwim-rules' for description of RANGE-TYPE."
  '((param . inner)
    (compound . outer)))

;;;;; clone rules

(defun evil-ts-obj-def-clone-rules (_range-type)
  "Return default clone rules.
See `evil-ts-obj-conf-clone-rules' for description of RANGE-TYPE."
  '((param . outer)
    (statement . outer)
    (compound . outer)))

(defun evil-ts-obj-def-conf-lang-clone-rules (_range-type)
  "Return default clone rules for configuration languages like YAML.
See `evil-ts-obj-conf-clone-rules' for description of RANGE-TYPE."
  '((param . outer)
    (compound . outer)))

;;;;; extract rules

(defun evil-ts-obj-def-extract-rules (range-type)
  "Return default extract rules.
See `evil-ts-obj-conf-extract-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('text
     '((param . inner)
       (statement . inner)
       (compound . outer)))
    ('place
     '((statement . inner)
       (compound . outer)))))

(defun evil-ts-obj-def-conf-lang-extract-rules (range-type)
  "Return default extract rules for configuration languages like YAML.
See `evil-ts-obj-conf-extract-rules' for description of RANGE-TYPE."

  (pcase range-type
    ('text
     '((param . inner)
       (compound . outer)))
    ('place
     '((param . inner)))))

;;;;; inject rules

(defun evil-ts-obj-def-inject-rules (range-type)
  "Return default inject rules.
See `evil-ts-obj-conf-inject-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('text
     '((statement . inner)
       (compound . outer)))
    ('place
     '((compound . inner)))))

(defun evil-ts-obj-def-conf-lang-inject-rules (range-type)
  "Return default inject rules for configuration languages like YAML.
See `evil-ts-obj-conf-inject-rules' for description of RANGE-TYPE."

  (pcase range-type
    ('text
     '((param . inner)))
    ('place
     '((param-compound . inner)))))

;;;;; slurp rules

(defun evil-ts-obj-def-slurp-rules (range-type)
  "Return default slurp rules.
See `evil-ts-obj-conf-slurp-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('place
     '((compound . inner)))
    ('text
     '((statement . inner)
       (compound . outer)))))

(defun evil-ts-obj-def-conf-lang-slurp-rules (range-type)
  "Return default slurp rules for configuration languages like YAML.
See `evil-ts-obj-conf-slurp-rules' for description of RANGE-TYPE."

  (pcase range-type
    ('place
     '((param-compound . inner)))
    ('text
     '((param . inner)))))

;;;;; barf rules

(defun evil-ts-obj-def-barf-rules (range-type)
  "Return default barf rules.
See `evil-ts-obj-conf-barf-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('place
     '((compound . outer)))
    ('text
     '((statement . inner)
       (compound . outer)))))

(defun evil-ts-obj-def-conf-lang-barf-rules (range-type)
  "Return default barf rules for configuration languages like YAML.
See `evil-ts-obj-conf-barf-rules' for description of RANGE-TYPE."

  (pcase range-type
    ('place
     '((param-compound . outer)))
    ('text
     '((param . inner)))))


;;;;; convolute rules

(defun evil-ts-obj-def-convolute-rules (range-type)
  "Return default convolute rules.
See `evil-ts-obj-conf-convolute-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('parent
     '((statement . inner)
       (compound . outer)))
    ('text
     '((param . inner)
       (statement . inner)
       (compound . outer)))))

(defun evil-ts-obj-def-conf-lang-convolute-rules (range-type)
  "Return default convolute rules for configuration languages like YAML.
See `evil-ts-obj-conf-convolute-rules' for description of RANGE-TYPE."
  (pcase range-type
    ('parent
     '((param-compound . outer)))
    ('text
     '((param . inner)
       (compound . outer)))))


;;;; sibling traverse helpers

(defvar evil-ts-obj-def--sibling-trav (evil-ts-obj-trav-create
                                       :fetcher #'evil-ts-obj--get-sibling-simple
                                       :kind-func #'evil-ts-obj--get-node-kind)
  "Default value for sibling traversing.")

(defun evil-ts-obj-def--create-sibling-trav (user-trav)
  "Create sibling traversing object combining USER-TRAV with the default one."
  (evil-ts-obj-trav-create
   :fetcher (or (evil-ts-obj-trav-fetcher user-trav)
                (evil-ts-obj-trav-fetcher evil-ts-obj-def--sibling-trav))
   :kind-func (or (evil-ts-obj-trav-kind-func user-trav)
                  (evil-ts-obj-trav-kind-func evil-ts-obj-def--sibling-trav))))


;;;; init functions
(cl-defun evil-ts-obj-def-init-lang (
                                     lang things &optional &key
                                     (ext-func nil)
                                     (param-seps nil)
                                     (statement-seps nil)
                                     (nav-thing '(or param statement compound))
                                     (compound-sib-trav evil-ts-obj-def--sibling-trav)
                                     (statement-sib-trav evil-ts-obj-def--sibling-trav)
                                     (param-sib-trav evil-ts-obj-def--sibling-trav)
                                     (terms nil)
                                     (clone-indent-policy 'cur-indent)
                                     (compound-brackets nil)
                                     (statement-placeholder nil))
  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable. If THINGS does
not contain a thing for the comment, then simle regexp \"^comment$\"  is
added as the comment thing.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

PARAM-SEPS and STATEMENT-SEPS are lists of separators for current
language. They are added to `evil-ts-obj-conf-seps', along with
new list with key \\='all that is concatenation of provided
separator lists.

NAV-THING is added to `evil-ts-obj-conf-nav-things', default
value: (or param statement compound).

COMPOUND-SIB-TRAV, STATEMENT-SIB-TRAV, PARAM-SIB-TRAV define
sibling traversing. They are objects of type `evil-ts-obj-trav'.
An object can be instantiated using `evil-ts-obj-trav-create'. It
accepts following arguments: :fetcher - a function for fetching
next/previous sibling (default
`evil-ts-obj--get-sibling-simple'), :kind-func - a function for
labeling a fetched node (default `evil-ts-obj--get-node-kind').
It is possible to leave some slots undefined. In that case the
default values will be used. An object will be added to
`evil-ts-obj-conf-sibling-trav' variable, see description of this
variable for more information about functions.

TERMS is added to `evil-ts-obj-conf-terms', default value is nil.
CLONE-INDENT-POLICY is used to populate
`evil-ts-obj-conf-clone-indent-policy', default value is
cur-indent. COMPOUND-BRACKETS and STATEMENT-PLACEHOLDER are used
by extract and inject operators. They populate
`evil-ts-obj-conf-compound-brackets' and
`evil-ts-obj-conf-statement-placeholder' respectively. Both
values are nil by default.

This function also adds `evil-ts-obj--finalize-text-obj-range' to
`evil-ts-obj-conf-range-finalizers',
`evil-ts-obj-def-raise-rules' to `evil-ts-obj-conf-raise-rules'.
And other default rules to its corresponding variables."


  (make-local-variable 'treesit-thing-settings)
  (when (not (assq 'comment things))
    (cl-callf append things '((comment "^comment$"))))

  (cl-callf append (alist-get lang treesit-thing-settings) things)

  (when ext-func
    (cl-callf plist-put evil-ts-obj-conf-thing-modifiers lang ext-func))

  (when-let ((all-seps (append param-seps statement-seps)))
    (cl-callf plist-put evil-ts-obj-conf-seps lang
              `(all ,all-seps
                param ,param-seps
                statement ,statement-seps)))

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

  (cl-callf plist-put evil-ts-obj-conf-terms lang terms)

  (cl-callf plist-put evil-ts-obj-conf-range-finalizers lang #'evil-ts-obj--finalize-text-obj-range)
  (cl-callf plist-put evil-ts-obj-conf-raise-rules lang #'evil-ts-obj-def-raise-rules)
  (cl-callf plist-put evil-ts-obj-conf-drag-rules lang #'evil-ts-obj-def-drag-rules)
  (cl-callf plist-put evil-ts-obj-conf-swap-dwim-rules lang #'evil-ts-obj-def-swap-dwim-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-rules lang #'evil-ts-obj-def-clone-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-indent-policy lang clone-indent-policy)

  (cl-callf plist-put evil-ts-obj-conf-extract-rules lang #'evil-ts-obj-def-extract-rules)
  (cl-callf plist-put evil-ts-obj-conf-compound-brackets lang compound-brackets)
  (cl-callf plist-put evil-ts-obj-conf-statement-placeholder lang statement-placeholder)
  (cl-callf plist-put evil-ts-obj-conf-inject-rules lang #'evil-ts-obj-def-inject-rules)
  (cl-callf plist-put evil-ts-obj-conf-slurp-rules lang #'evil-ts-obj-def-slurp-rules)
  (cl-callf plist-put evil-ts-obj-conf-barf-rules lang #'evil-ts-obj-def-barf-rules)
  (cl-callf plist-put evil-ts-obj-conf-convolute-rules lang #'evil-ts-obj-def-convolute-rules))


(cl-defun evil-ts-obj-def-init-conf-lang (
                                          lang things &optional &key
                                          (ext-func nil)
                                          (param-seps nil)
                                          (nav-thing '(or param compound))
                                          (compound-sib-trav evil-ts-obj-def--sibling-trav)
                                          (param-sib-trav evil-ts-obj-def--sibling-trav)
                                          (clone-indent-policy nil))

  "Set default values for language LANG.
THINGS are added to `treesit-thing-settings' variable. If THINGS does
not contain a thing for the comment, then simle regexp \"^comment$\" is
added as the comment thing.


Possible keyword values: If EXT-FUNC is not nil, it is added
`evil-ts-obj-conf-thing-modifiers' variable.

PARAM-SEPS are separators for current language.

NAV-THING - added to `evil-ts-obj-conf-nav-things', default
value: (or param compound).

For information about COMPOUND-SIB-TRAV, PARAM-SIB-TRAV see
`evil-ts-obj-def-init-lang'.

This function also adds `evil-ts-obj-def-raise-rules' to
`evil-ts-obj-def-conf-lang-raise-rules'."

  (make-local-variable 'treesit-thing-settings)
  (when (not (assq 'comment things))
    (cl-callf append things '((comment "^comment$"))))

  (cl-callf append (alist-get lang treesit-thing-settings) things)

  (when ext-func
    (cl-callf plist-put evil-ts-obj-conf-thing-modifiers lang ext-func))

  (when param-seps
    (cl-callf plist-put evil-ts-obj-conf-seps lang
              `(all ,param-seps
                param ,param-seps)))

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
  (cl-callf plist-put evil-ts-obj-conf-swap-dwim-rules
    lang #'evil-ts-obj-def-conf-lang-swap-dwim-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-rules
    lang #'evil-ts-obj-def-conf-lang-clone-rules)
  (cl-callf plist-put evil-ts-obj-conf-clone-indent-policy lang clone-indent-policy)
  (cl-callf plist-put evil-ts-obj-conf-extract-rules lang
            #'evil-ts-obj-def-conf-lang-extract-rules)
  (cl-callf plist-put evil-ts-obj-conf-inject-rules lang
            #'evil-ts-obj-def-conf-lang-inject-rules)
  (cl-callf plist-put evil-ts-obj-conf-slurp-rules lang
            #'evil-ts-obj-def-conf-lang-slurp-rules)
  (cl-callf plist-put evil-ts-obj-conf-barf-rules lang
            #'evil-ts-obj-def-conf-lang-barf-rules)
  (cl-callf plist-put evil-ts-obj-conf-convolute-rules lang
            #'evil-ts-obj-def-conf-lang-convolute-rules))


(provide 'evil-ts-obj-def)
;;; evil-ts-obj-def.el ends here
