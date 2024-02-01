;;; evil-ts-obj-cpp.el --- Bash setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
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
;;  cpp setting for evil-ts-obj
;;
;;; Code:


(require 'evil-ts-obj-def)

(defcustom evil-ts-obj-cpp-compound-nodes
  '("struct_specifier"
    "function_definition"
    "if_statement"
    "else_clause"
    "while_statement"
    "for_statement"
    "switch_statement"
    "case_statement"

    "template_declaration"
    "class_specifier"
    "for_range_loop"
    "try_statement"
    "catch_clause")
  "Nodes that designate compound statement in cpp."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defun evil-ts-obj-cpp-compound-pred (node)
  "Match only top-level template_declaration.
This is useful if NODE represents struct, class or function."
  (if-let* ((parent (treesit-node-parent node))
            ((equal (treesit-node-type parent) "template_declaration")))
      nil
    t))

(defvar evil-ts-obj-cpp-statement-regex nil
  "Regex is composed from `evil-ts-obj-cpp-statement-nodes'.")

(defcustom evil-ts-obj-cpp-statement-nodes
  '("type_definition"
    "field_declaration"
    "declaration"
    "expression_statement"
    "return_statement"
    "call_expression"

    "using_declaration"
    "alias_declaration"
    "throw_statement")
  "Nodes that designate simple statement in cpp."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-cpp-statement-seps
  '("&&" "||" "and" "or")
  "Separators for cpp statements."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defun evil-ts-obj-cpp-statement-pred (node)
  "Return t if NODE is a statement thing.
Consider NODE to be a statement if it is used as condition in a
compound statement or it is a part of a boolean expression, or if
its type is matched against `evil-ts-obj-cpp-statement-regex'."

  (or (and (equal (treesit-node-type (treesit-node-parent node)) "condition_clause")
           (equal (treesit-node-field-name node) "value"))
      (evil-ts-obj--common-bool-expr-pred node "binary_expression"
                                          evil-ts-obj-cpp-statement-seps)
      (string-match-p evil-ts-obj-cpp-statement-regex (treesit-node-type node))))


(defvar evil-ts-obj-cpp-param-parent-regex nil
  "Regex is composed from `evil-ts-obj-cpp-param-parent-nodes'.")

(defcustom evil-ts-obj-cpp-param-parent-nodes
  '("parameter_list"
    "argument_list"

    "template_parameter_list"
    "template_argument_list"
    "initializer_list"
    "lambda_capture_specifier")
  "Parent nodes for a parameter thing in python."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-cpp-param-seps
  '(",")
  "Separators for cpp params."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-cpp-things
  `((compound ,(cons (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-cpp-compound-nodes)
                     #'evil-ts-obj-cpp-compound-pred))
    (statement evil-ts-obj-cpp-statement-pred)
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-cpp-param-parent-regex n))))
  "Things for cpp."
  :type 'plist
  :group 'evil-ts-obj)


(defun evil-ts-obj-cpp--find-func-in-template (node)
  "Find function, class or struct child and return it.
NODE should have type template_declaration."
  (let* ((child-node (treesit-node-child node -1))
         (child-type (treesit-node-type child-node)))
    (when (equal child-type ";")
      ;; class/sruct
      (setq child-node (treesit-node-child node -2)
            child-type (treesit-node-type child-node)))
    (if (not (equal child-type "template_declaration"))
        child-node
      (evil-ts-obj-cpp--find-func-in-template child-node))))

(defun evil-ts-obj-cpp-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let ((body-node
              (pcase (treesit-node-type node)
                ("if_statement"
                 (treesit-node-child-by-field-name node "consequence"))
                ((or "else_clause" "case_statement")
                 (treesit-node-child node -1 t))
                ("template_declaration"
                 (thread-first node
                               (evil-ts-obj-cpp--find-func-in-template)
                               (treesit-node-child-by-field-name "body")))
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

(defun evil-ts-obj-cpp-compound-outer-ext (node)
  "Extend a struct/class range to include trailing ;.
Current thing is represented by `NODE'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))
    (when-let* (((member (treesit-node-type node) '("struct_specifier" "class_specifier")))
                (next-node (treesit-node-next-sibling node))
                ((equal (treesit-node-type next-node) ";")))
      (setq end (treesit-node-end next-node)))
    (list start end)))

(defun evil-ts-obj-cpp-compound-nav-mod (node)
  "Skip templates header when navigating to templated things.
If NODE has type template_declaration find function, class or
struct child and jump to its beginning."
  (when (equal (treesit-node-type node) "template_declaration")
    (let ((func-node (evil-ts-obj-cpp--find-func-in-template node)))
      (list (treesit-node-start func-node)
            (treesit-node-end func-node)))))

(defun evil-ts-obj-cpp-ext (spec node)
  "Main extension function for cpp.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-cpp-extract-compound-inner node))
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'op))
     (evil-ts-obj-cpp-compound-outer-ext node))
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'nav))
     (evil-ts-obj-cpp-compound-nav-mod node))))

(defcustom evil-ts-obj-cpp-ext-func
  #'evil-ts-obj-cpp-ext
  "Extension function for cpp."
  :type 'function
  :group 'evil-ts-obj)


;;;###autoload
(defun evil-ts-obj-cpp-setup-things ()
  "Set all variables needed by evil-ts-obj-core."
  (evil-ts-obj-def-init-lang 'cpp evil-ts-obj-cpp-things
                             :ext-func evil-ts-obj-cpp-ext-func
                             :param-seps evil-ts-obj-cpp-param-seps
                             :statement-seps evil-ts-obj-cpp-statement-seps
                             :terms '(";")
                             :statement-sib-trav
                             (evil-ts-obj-trav-create
                              :fetcher (lambda (d n) (evil-ts-obj--common-get-statement-sibling
                                                      d n '("binary_expression"))))
                             :compound-brackets "{}"))

(provide 'evil-ts-obj-cpp)
;;; evil-ts-obj-cpp.el ends here
