;;; evil-ts-obj-c.el --- Bash setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Version: 0.0.1
;; Keywords: convenience tools
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  c setting for evil-ts-obj
;;
;;; Code:


(require 'evil-ts-obj-def)

(defcustom evil-ts-obj-c-compound-nodes
  '("struct_specifier"
    "function_definition"
    "if_statement"
    "else_clause"
    "while_statement"
    "for_statement"
    "switch_statement"
    "case_statement")
  "Nodes that designate compound statement in c."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defvar evil-ts-obj-c-statement-regex nil
  "Regex is composed from `evil-ts-obj-c-statement-nodes'.")

(defcustom evil-ts-obj-c-statement-nodes
  '("type_definition"
    "field_declaration"
    "declaration"
    "expression_statement"
    "return_statement"
    "call_expression")
  "Nodes that designate simple statement in c."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-c-statement-seps
  '("&&" "||")
  "Separators for c statements."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defun evil-ts-obj-c-statement-pred (node)
  "Return t if NODE is a statement thing.
Consider NODE to be a statement if it is used as condition in a
compound statement or it is a part of a boolean expression, or if
its type is matched against `evil-ts-obj-c-statement-regex'."

  (or (evil-ts-obj--by-field-name-pred node '(;; rhs
                                              ("init_declarator" . "value")
                                              ("assignment_expression" . "right")))
      ;; the whole condition
      (when-let* (((treesit-node-check node 'named))
                  (parent (treesit-node-parent node))
                  ((equal (treesit-node-type parent) "parenthesized_expression"))
                  ((equal (treesit-node-field-name parent) "condition")))
        t)
      ;; parts of boolean expressions
      (evil-ts-obj--common-bool-expr-pred node "binary_expression"
                                          evil-ts-obj-c-statement-seps)
      (string-match-p evil-ts-obj-c-statement-regex (treesit-node-type node))))


(defvar evil-ts-obj-c-param-parent-regex nil
  "Regex is composed from `evil-ts-obj-c-param-parent-nodes'.")

(defcustom evil-ts-obj-c-param-parent-nodes
  '("parameter_list"
    "argument_list")
  "Parent nodes for a parameter thing in python."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-c-param-seps
  '(",")
  "Separators for c params."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-c-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-c-compound-nodes))
    (statement evil-ts-obj-c-statement-pred)
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-c-param-parent-regex n))))
  "Things for c."
  :type 'plist
  :group 'evil-ts-obj)



(defun evil-ts-obj-c-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let ((body-node
              (pcase (treesit-node-type node)
                ("if_statement"
                 (treesit-node-child-by-field-name node "consequence"))
                ((or "else_clause" "case_statement")
                 (treesit-node-child node -1 t))
                (_
                 (treesit-node-child-by-field-name node "body")))))
    (if (member (treesit-node-type body-node) '("compound_statement"
                                                "field_declaration_list"))
        ;; do not include enclosing braces
        (if-let ((first-child (treesit-node-child body-node 0 t))
                 (last-child  (treesit-node-child body-node -1 t)))
            (list (treesit-node-start first-child)
                  (treesit-node-end last-child))
          ;; empty body
          (list (treesit-node-end (treesit-node-child body-node 0))
                (treesit-node-start (treesit-node-child body-node 1))))

      (list (treesit-node-start body-node)
            (treesit-node-end body-node)))))

(defun evil-ts-obj-c-compound-outer-ext (node)
  "Extend a struct/class range to include trailing ;.
Current thing is represented by `NODE'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (when-let* (((member (treesit-node-type node) '("struct_specifier" "class_specifier")))
                (next-node (treesit-node-next-sibling node))
                ((equal (treesit-node-type next-node) ";")))
      (setq end (treesit-node-end next-node)))
    (list start end)))


(defun evil-ts-obj-c-ext (spec node)
  "Main extension function for c.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-c-extract-compound-inner node))
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'op))
     (evil-ts-obj-c-compound-outer-ext node))))

(defcustom evil-ts-obj-c-ext-func
  #'evil-ts-obj-c-ext
  "Extension function for c."
  :type 'function
  :group 'evil-ts-obj)


;;;###autoload
(defun evil-ts-obj-c-setup-things ()
  "Set all variables needed by evil-ts-obj-core."
  (evil-ts-obj-def-init-lang 'c evil-ts-obj-c-things
                             :ext-func evil-ts-obj-c-ext-func
                             :param-seps evil-ts-obj-c-param-seps
                             :statement-seps evil-ts-obj-c-statement-seps
                             :terms '(";")
                             :statement-sib-trav
                             (evil-ts-obj-trav-create
                              :fetcher (lambda (d n) (evil-ts-obj--common-get-statement-sibling
                                                      d n '("binary_expression"))))
                             :compound-brackets "{}"))

(provide 'evil-ts-obj-c)
;;; evil-ts-obj-c.el ends here
