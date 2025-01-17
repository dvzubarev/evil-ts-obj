;;; evil-ts-obj-python.el --- Python setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
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

(require 'evil-ts-obj-def)


(defcustom evil-ts-obj-python-compound-nodes
  '("class_definition"
    "decorated_definition"
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

(defun evil-ts-obj-python-compound-pred (node)
  "Predicate for a compound thing.
When NODE is a function and it has decorator, match only top
level decorated_definition node as a thing."
  (if-let* (((equal (treesit-node-type node) "function_definition"))
            (parent (treesit-node-parent node))
            ((equal (treesit-node-type parent) "decorated_definition")))
      nil
    t))

(defvar evil-ts-obj-python-statement-regex nil
  "Regex is composed from `evil-ts-obj-python-statement-nodes'.")

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
    "dictionary_comprehension"
    "break_statement"
    "continue_statement")
  "Nodes that designate simple statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defun evil-ts-obj-python--looks-like-docstring? (node)
  "Return t if NODE is an expression that with only string node."
  (and (equal (treesit-node-type node) "expression_statement")
       (equal (treesit-node-type (treesit-node-child node 0)) "string")))

(defun evil-ts-obj-python-statement-pred (node)
  "Return t if NODE is a statement thing.
Consider NODE to be a statement if it is used as a condition in a
compound statement or it is a part of a boolean expression, or if
its type is matched against `evil-ts-obj-python-statement-regex'."
  (or (evil-ts-obj--by-field-name-pred node '((nil . "condition")
                                              ("assignment" . "right")
                                              ("interpolation" . "expression")))
      ;; expresions in return/yield
      (and (treesit-node-check node 'named)
           (member (treesit-node-type (treesit-node-parent node)) '("return_statement" "yield")))
      ;; parts of boolean expression
      (and (not (equal (treesit-node-type node) "boolean_operator"))
           (equal (treesit-node-type (treesit-node-parent node)) "boolean_operator")
           (member (treesit-node-field-name node) '("left" "right")))
      (and
       ;; Do not match strings as statements, they are used as docstrings.
       (not (evil-ts-obj-python--looks-like-docstring? node))
       (string-match-p evil-ts-obj-python-statement-regex (treesit-node-type node)))))

(defvar evil-ts-obj-python-param-parent-regex nil
  "Regex is composed from `evil-ts-obj-python-param-parent-nodes'.")

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
  `((compound ,(cons (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-compound-nodes)
                     #'evil-ts-obj-python-compound-pred))
    (statement evil-ts-obj-python-statement-pred)
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-python-param-parent-regex n)))
    (str "^string$"))
  "Things for python."
  :type 'plist
  :group 'evil-ts-obj)


(defcustom evil-ts-obj-python-statement-seps
  '(";" "and" "or")
  "Separators for python statements."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defcustom evil-ts-obj-python-param-seps
  '(",")
  "Separators for python params."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defun evil-ts-obj-python-compound-nav-mod (node)
  "Skip decorator header when navigating to decorated function.
If NODE has type decorated_definition, find function_definition
child and jump to its beginning."
  (when (equal (treesit-node-type node) "decorated_definition")
    (when-let (func-node (treesit-node-child-by-field-name node "definition"))
      func-node)))

(defun evil-ts-obj-python-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let* ((node-t (treesit-node-type node))
              (block-node
               (pcase node-t
                 ((or "if_statement" "elif_clause" "case_clause")
                  (treesit-node-child-by-field-name node "consequence"))
                 ((or "except_clause" "finally_clause")
                  (treesit-node-child node -1))
                 ("decorated_definition"
                  (thread-first node
                                (treesit-node-child-by-field-name "definition")
                                (treesit-node-child-by-field-name "body")))
                 (_
                  (treesit-node-child-by-field-name node "body"))))
              ((equal (treesit-node-type block-node) "block")))

    (let* ((first-inner (treesit-node-child block-node 0 t))
           (last-inner (treesit-node-child block-node -1 t))
           (node-range (evil-ts-obj-node-range-create (or first-inner block-node)
                                                      (or last-inner block-node))))
      ;; exclude docstring from the inner object range.
      (when (and (member node-t '("function_definition" "decorated_definition" "class_definition"))
                 (evil-ts-obj-python--looks-like-docstring? first-inner))
        (if-let* ((next-after-docstring (treesit-node-child block-node 1 t)))
            (evil-ts-obj-node-range-change-start node-range :node next-after-docstring)
          ;; Block contains only docstring, so make empty node range.
          (evil-ts-obj-node-range-change-start node-range :exclude t)))
      node-range)))

(defun evil-ts-obj-python-ext (spec node)
  "Main extension function for python.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-python-extract-compound-inner node))
    ((pmap (:thing 'str) (:mod 'inner))
     (evil-ts-obj-string-inner-py-style node))
    ((pmap (:thing 'compound) (:act 'nav))
     (evil-ts-obj-python-compound-nav-mod node))))

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
                             :statement-seps evil-ts-obj-python-statement-seps
                             :statement-sib-trav
                             (evil-ts-obj-trav-create
                              :fetcher (lambda (d n) (evil-ts-obj--common-get-statement-sibling
                                                      d n '("boolean_operator")
                                                      evil-ts-obj-python-statement-seps)))
                             :statement-placeholder '("pass" "...")))



(provide 'evil-ts-obj-python)
;;; evil-ts-obj-python.el ends here
