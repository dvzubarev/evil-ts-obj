;;; evil-ts-obj-rust.el --- Rust setting for evil-ts-obj -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;  rust setting for evil-ts-obj
;;
;;; Code:


(require 'evil-ts-obj-def)

(defcustom evil-ts-obj-rust-compound-nodes
  '("struct_item"
    "enum_item"
    "trait_item"
    "union_item"
    "impl_item"
    "function_item"
    "closure_expression"
    "if_expression"
    "else_clause"
    "while_expression"
    "for_expression"
    "loop_expression"
    "match_expression"
    "expression_statement"
    "match_arm")
  "Nodes that designate compound statement in rust."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defvar evil-ts-obj-rust-statement-regex nil
  "Regex is composed from `evil-ts-obj-rust-statement-nodes'.")

(defcustom evil-ts-obj-rust-statement-nodes
  '("type_item"
    "const_item"
    "static_item"
    "field_declaration"
    "let_declaration"
    "expression_statement"
    "return_expression"
    "call_expression")
  "Nodes that designate simple statement in rust."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-rust-statement-seps
  '("&&" "||")
  "Separators for rust statements."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defun evil-ts-obj-rust-compound-pred (node)
  "Return t if NODE is a compound thing.
Consider NODE to be a compound if it is an expression_statement holding
other compounds."
  (let ((res
  (pcase (treesit-node-type node)
    ("expression_statement"
     (when-let ((child (treesit-node-child node 0))
                (child-type (treesit-node-type child)))
       (member child-type evil-ts-obj-rust-compound-nodes)))
    (_
     t))))
    res))

(defun evil-ts-obj-rust-statement-pred (node)
  "Return t if NODE is a statement thing.
Consider NODE to be a statement if it is used as condition in a
compound statement or it is a part of a boolean expression, or if
its type is matched against `evil-ts-obj-rust-statement-regex'."

  (or (evil-ts-obj--by-field-name-pred node '((nil . "condition")
                                              ;; rhs
                                              ("let_declaration" . "value")
                                              ("assignment_expression" . "right")))
      ;; parts of boolean expressions
      (evil-ts-obj--common-bool-expr-pred node "binary_expression"
                                          evil-ts-obj-rust-statement-seps)
      (string-match-p evil-ts-obj-rust-statement-regex (treesit-node-type node))))


(defvar evil-ts-obj-rust-param-parent-regex nil
  "Regex is composed from `evil-ts-obj-rust-param-parent-nodes'.")

(defcustom evil-ts-obj-rust-param-parent-nodes
  '("parameters"
    "closure_parameters"
    "arguments"
    "type_arguments"
    "type_parameters"
    "array_expression"
    "where_clause"
    "token_tree") ;; best effort to support function-call like macro invocations, like println!(),  vec![], #[attr()]
  "Parent nodes for a parameter thing in rust."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defcustom evil-ts-obj-rust-param-seps
  '(",")
  "Separators for rust params."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-rust-things
  `((compound ,(cons (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-rust-compound-nodes) #'evil-ts-obj-rust-compound-pred))
    (statement evil-ts-obj-rust-statement-pred)
    (param ,(lambda (n) (evil-ts-obj-common-param-pred evil-ts-obj-rust-param-parent-regex n))))
  "Things for rust."
  :type 'plist
  :group 'evil-ts-obj)


(defun evil-ts-obj-rust-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (if (equal (treesit-node-type node) "expression_statement")
    (evil-ts-obj-rust-extract-compound-inner (treesit-node-child node -1))
    (when-let ((body-node
              (pcase (treesit-node-type node)
                ("if_expression"
                 (treesit-node-child-by-field-name node "consequence"))
                ((or "else_clause" "match_arm")
                 (treesit-node-child node -1 t))
                (_
                 (treesit-node-child-by-field-name node "body")))))
    (if (member (treesit-node-type body-node) '("field_declaration_list" ;; struct/enum/trait fields
                                                "declaration_list" ;; impl blocks
                                                "block")) ;; bare { ... } blocks
        ;; do not include enclosing braces
        (if-let ((first-child (treesit-node-child body-node 0 t))
                 (last-child  (treesit-node-child body-node -1 t)))
            (list (treesit-node-start first-child)
                  (treesit-node-end last-child))
          ;; empty body
          (list (treesit-node-end (treesit-node-child body-node 0))
                (treesit-node-start (treesit-node-child body-node 1))))

      (list (treesit-node-start body-node)
            (treesit-node-end body-node))))))

(defun evil-ts-obj-rust-compound-outer-ext (node)
  "Extend a compound range to include preceding attributes.
Current thing is represented by `NODE'."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (prev-node (treesit-node-prev-sibling node)))
    (while (equal (treesit-node-type prev-node) "attribute_item")
      (setq start (treesit-node-start prev-node))
      (setq prev-node (treesit-node-prev-sibling prev-node)))
    (list start end)))

(defun evil-ts-obj-rust-ext (spec node)
  "Main extension function for rust.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-rust-extract-compound-inner node))
    ((pmap (:thing 'compound) (:mod 'outer) (:act 'op))
     (evil-ts-obj-rust-compound-outer-ext node))))


(defcustom evil-ts-obj-rust-ext-func
  #'evil-ts-obj-rust-ext
  "Extension function for rust."
  :type 'function
  :group 'evil-ts-obj)


;;;###autoload
(defun evil-ts-obj-rust-setup-things ()
  "Set all variables needed by evil-ts-obj-rustore."
  (evil-ts-obj-def-init-lang 'rust evil-ts-obj-rust-things
                             :ext-func evil-ts-obj-rust-ext-func
                             :param-seps evil-ts-obj-rust-param-seps
                             :statement-seps evil-ts-obj-rust-statement-seps
                             :terms '(";")
                             :statement-sib-trav
                             (evil-ts-obj-trav-create
                              :fetcher (lambda (d n) (evil-ts-obj--common-get-statement-sibling
                                                      d n '("binary_expression"))))
                             :compound-brackets "{}"))

(provide 'evil-ts-obj-rust)
;;; evil-ts-obj-rust.el ends here
