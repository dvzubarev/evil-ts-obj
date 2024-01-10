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

    "class_specifier"
    "for_range_loop"
    "try_statement"
    "catch_clause")
  "Nodes that designate compound statement in cpp."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-cpp-statement-nodes
  '("type_definition"
    "field_declaration"
    "declaration"
    "expression_statement"
    "return_statement"

    "using_declaration"
    "alias_declaration"
    "throw_statement")
  "Nodes that designate simple statement in cpp."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defvar evil-ts-obj-cpp-param-parent-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

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


(defcustom evil-ts-obj-cpp-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-cpp-compound-nodes))
    (statement ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-cpp-statement-nodes))
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-cpp-param-parent-regex n))))
  "Things for cpp."
  :type 'plist
  :group 'evil-ts-obj)

(defvar evil-ts-obj-cpp-param-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defcustom evil-ts-obj-cpp-param-seps
  ","
  "Separators for cpp params."
  :type '(choice (repeat string) string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-seps-setter)

(defun evil-ts-obj-cpp-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let* ((body-node
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
        (list (treesit-node-start (treesit-node-child body-node 0 t))
              (treesit-node-end (treesit-node-child body-node -1 t)))
      (list (treesit-node-start body-node)
            (treesit-node-end body-node)))))

(defun evil-ts-obj-cpp-compound-outer-ext (node)
  "Extend a struct/class range to include trailing ;.
If a parent for `NODE' is template_declaration extend to start of
the template_declaration. Current thing is represented by `NODE'."

  (let ((start (treesit-node-start node))
        (end (treesit-node-end node)))


    (when-let* (((member (treesit-node-type node) '("struct_specifier" "class_specifier")))
                (next-node (treesit-node-next-sibling node))
                ((equal (treesit-node-type next-node) ";")))
      (setq end (treesit-node-end next-node)))

    (when-let* ((parent (treesit-parent-while
                         (treesit-node-parent node)
                         (lambda (n)
                           (equal (treesit-node-type n) "template_declaration")))))
      (setq start (treesit-node-start parent)))

    (list start end)))

(defun evil-ts-obj-cpp-ext (spec node)
  "Main extension function for cpp.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'op))
     (evil-ts-obj-cpp-compound-outer-ext node))
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-cpp-extract-compound-inner node))))

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
                             :seps-reg evil-ts-obj-cpp-param-seps-regex
                             :param-sib-trav (evil-ts-obj-trav-create
                                              :seps evil-ts-obj-cpp-param-seps-regex)))

(provide 'evil-ts-obj-cpp)
;;; evil-ts-obj-cpp.el ends here
