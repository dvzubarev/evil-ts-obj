;;; evil-ts-obj-python.el --- Python setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Version: 0.0.1
;; Keywords: convenience tools
;; Homepage: https://github.com/dvzubarev/evil-ts-obj-python
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  python setting for evil-ts-obj
;;
;;; Code:

(require 'treesit)
(require 'evil-ts-obj-conf)
(require 'evil-ts-obj-core)


(defcustom evil-ts-obj-python-compound-nodes
  '("class_definition"
    "function_definition"
    "if_statement"
    "elif_clause"
    "else_clause"
    "while_statement"
    "for_statement"
    "with_statement"
    "try_statement"
    "except_clause"
    "finally_clause"
    "match_statement"
    "case_clause")
  "Nodes that designate compound statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defvar evil-ts-obj-python-statement-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

(defcustom evil-ts-obj-python-statement-nodes
  '("return_statement"
    "pass_statement"
    "expression_statement"
    "named_expression"
    "assert_statement"
    "delete_statement"
    "raise_statement"
    "call"
    "conditional_expression"
    "list_comprehension"
    "for_in_clause"
    "if_clause"
    "dictionary_comprehension")
  "Nodes that designate simple statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)





(defun evil-ts-obj-python-statement-pred (node)
  (if (or (equal (treesit-node-field-name node) "condition")
          (and (not (equal (treesit-node-type node) "boolean_operator"))
               (equal (treesit-node-type (treesit-node-parent node)) "boolean_operator")
               (member (treesit-node-field-name node) '("left" "right"))))
      t
    (string-match-p evil-ts-obj-python-statement-regex (treesit-node-type node))))

(defvar evil-ts-obj-python-param-parent-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

(defcustom evil-ts-obj-python-param-parent-nodes
  '("parameters"
    "lambda_parameters"
    "argument_list"
    "expression_list"
    "pattern_list"
    "list_pattern"
    "tuple_pattern"
    "dictionary"
    "list"
    "tuple")
  "Parent nodes for a parameter thing in python."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)


(defcustom evil-ts-obj-python-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-compound-nodes))
    (statement evil-ts-obj-python-statement-pred)
    (param ,(apply-partially #'evil-ts-obj-common-param-pred
                             evil-ts-obj-python-param-parent-regex)))
  "Things for python."
  :type 'plist
  :group 'evil-ts-obj)

(defvar evil-ts-obj-python-all-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defvar evil-ts-obj-python-statement-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defcustom evil-ts-obj-python-statement-seps
  '(";" "and" "or")
  "Separators for python statements."
  :type '(choice (repeat string) string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-seps-setter)


(defvar evil-ts-obj-python-param-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defcustom evil-ts-obj-python-param-seps
  ","
  "Separators for python params."
  :type '(choice (repeat string) string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-seps-setter)


(defun evil-ts-obj-python-compound-outer-ext (node)
  "Extend a function range to the start of decorator, if it exists.
Current thing is represented by `NODE'."
  (when-let* ((is-func (equal (treesit-node-type node) "function_definition"))
              (parent (treesit-node-parent node))
              (is-decorator (equal (treesit-node-type parent) "decorated_definition")))
    (list (treesit-node-start parent)
          (treesit-node-end parent))))

(defun evil-ts-obj-python-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let* ((block-node
               (pcase (treesit-node-type node)
                 ((or "if_statement" "elif_clause" "case_clause")
                  (treesit-node-child-by-field-name node "consequence"))
                 ((or "except_clause" "finally_clause")
                  (treesit-node-child node -1))
                (_
                 (treesit-node-child-by-field-name node "body"))))
              ((equal (treesit-node-type block-node) "block")))
    (list (treesit-node-start block-node)
          (treesit-node-end block-node))))

(defun evil-ts-obj-python-compound-sibling-kind (_cur-node _cur-kind node)
  (unless (equal (treesit-node-type node) ":")
    'sibling))

(defun evil-ts-obj-python-statement-get-sibling (dir node)

  (if-let* ((sibling (evil-ts-obj--get-sibling-bin-op '("boolean_operator") dir node)))
      sibling
    (evil-ts-obj--get-sibling-simple dir node)))

(defun evil-ts-obj-python-ext (spec node)
  "Main extension function for python.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-python-extract-compound-inner node))
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'op))
     (evil-ts-obj-python-compound-outer-ext node))
    ((pmap (:thing 'compound) (:mod 'upper) (:act 'op))
     (evil-ts-obj-generic-thing-upper
      node
      #'evil-ts-obj-python-compound-sibling-kind
      #'evil-ts-obj--get-sibling-simple))

    ((pmap (:act 'op) (:thing 'statement))
     (evil-ts-obj-common-statement-ext
      spec node
      evil-ts-obj-python-statement-seps-regex
      #'evil-ts-obj-python-statement-get-sibling))

    ((pmap (:act 'op)  (:thing 'param))
     (evil-ts-obj-common-param-ext spec node evil-ts-obj-python-param-seps))))

(defcustom evil-ts-obj-python-ext-func
  #'evil-ts-obj-python-ext
  "Extension function for python."
  :type 'function
  :group 'evil-ts-obj)

;;;###autoload
(defun evil-ts-obj-python-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (evil-ts-obj-conf-init-default 'python)

  (make-local-variable 'treesit-thing-settings)
  (cl-callf append (alist-get 'python treesit-thing-settings)
    evil-ts-obj-python-things)

  (cl-callf plist-put evil-ts-obj-conf-thing-modifiers
   'python evil-ts-obj-python-ext-func)

  (cl-callf plist-put evil-ts-obj-conf-sep-regexps 'python evil-ts-obj-python-all-seps-regex))



(provide 'evil-ts-obj-python)
;;; evil-ts-obj-python.el ends here
