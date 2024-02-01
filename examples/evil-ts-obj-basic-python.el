;;; evil-ts-obj-basic-python.el --- Basic Python setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Author:
;; Maintainer:
;; Version: 0.0.1
;; Keywords: convenience tools
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  python setting for evil-ts-obj
;;
;;; Code:

(require 'evil-ts-obj-def)


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
    "match_statement"
    "case_clause")
  "Nodes that designate compound statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defcustom evil-ts-obj-python-statement-nodes
  '("return_statement"
    "pass_statement"
    "expression_statement"
    "named_expression"
    "assert_statement"
    "delete_statement"
    "raise_statement"
    "call"
    "conditional_expression")
  "Nodes that designate simple statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj)


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
    (statement ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-statement-nodes))
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-python-param-parent-regex n))))
  "Things for python."
  :type 'plist
  :group 'evil-ts-obj)


(defcustom evil-ts-obj-python-param-seps
  '(",")
  "Separators for python params."
  :type '(repeat string)
  :group 'evil-ts-obj)


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



(defun evil-ts-obj-python-ext (spec node)
  "Main extension function for python.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-python-extract-compound-inner node))))

(defcustom evil-ts-obj-python-ext-func
  #'evil-ts-obj-python-ext
  "Extension function for python."
  :type 'function
  :group 'evil-ts-obj)

;;;###autoload
(defun evil-ts-obj-python-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (evil-ts-obj-def-init-lang 'python evil-ts-obj-python-things
                             :ext-func evil-ts-obj-python-ext-func
                             :param-seps evil-ts-obj-python-param-seps
                             :stmnt-add-sibl-rules nil))


(provide 'evil-ts-obj-basic-python)
;;; evil-ts-obj-basic-python.el ends here
