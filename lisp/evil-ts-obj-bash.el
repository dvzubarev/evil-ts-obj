;;; evil-ts-obj-bash.el --- Bash setting for evil-ts-obj -*- lexical-binding: t; -*-
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
;;  bash setting for evil-ts-obj
;;
;;; Code:

(require 'evil-ts-obj-def)

(defcustom evil-ts-obj-bash-compound-nodes
  '("function_definition"
    "if_statement"
    "elif_clause"
    "else_clause"
    "while_statement"
    "for_statement"
    "c_style_for_statement"
    "case_statement")
  "Nodes that designate compound statement in bash."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defvar evil-ts-obj-bash-statement-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

(defcustom evil-ts-obj-bash-statement-nodes
  '("command"
    "unset_command"
    "test_command"
    "declaration_command"
    "variable_assignment"
    "redirected_statement"
    "pipeline"
    "list")
  "Nodes that designate simple statement in bash."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defvar evil-ts-obj-bash-statement-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defcustom evil-ts-obj-bash-statement-seps
  '("|" ";" "||" "&&")
  "Separators for bash statements."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-seps-setter)

(defun evil-ts-obj-bash-statement-pred (node)
  "Predicate for detecting statement thing, represented by `NODE'.
Consider NODE to be a statement if it is used as condition in a
compound statement or it is a part of a boolean expression, or if
its type is matched against `evil-ts-obj-bash-statement-regex'.
For list node, it returns t only for the furthest parent of the
same type."

  (let ((node-type (treesit-node-type node))
        (parent-type (treesit-node-type (treesit-node-parent node))))
    (pcase node-type
      ("list" (not (equal node-type parent-type)))
      (_
       (or
        (and (equal parent-type "test_command") (treesit-node-check node 'named))
        (evil-ts-obj--common-bool-expr-pred node "binary_expression"
                                            evil-ts-obj-bash-statement-seps-regex)
        (string-match-p evil-ts-obj-bash-statement-regex (treesit-node-type node)))))))

(defun evil-ts-obj-bash-param-pred (node)
  "Predicate for detecting param thing.
Return t if `NODE' is a node that represents a parameter."
  (equal (treesit-node-field-name node) "argument"))

(defcustom evil-ts-obj-bash-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-bash-compound-nodes))
    (statement evil-ts-obj-bash-statement-pred)
    (param evil-ts-obj-bash-param-pred))
  "Things for bash."
  :type 'plist
  :group 'evil-ts-obj)


(defun evil-ts-obj-bash-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (let (first-child last-child)
    (pcase (treesit-node-type node)
      ((or "function_definition"
           "while_statement"
           "for_statement"
           "c_style_for_statement")
       (when-let ((body-node (treesit-node-child-by-field-name node "body")))
         (setq first-child (treesit-node-child body-node 0 t)
               last-child (treesit-node-child body-node -1 t))))
      ("if_statement"
                                        ;skip condition node
       (setq first-child (treesit-node-child node 1 t))
       ;; run till we are not on  elif_clause or else_clause
       (when-let* ((all-children (treesit-node-children node t))
                   (if-children (seq-take-while
                                 (lambda (n) (not (member (treesit-node-type n)
                                                          '("elif_clause" "else_clause"))))
                                 all-children)))
         (setq last-child (car (last if-children)))))
      ((or "elif_clause" "case_statement" "else_clause")
       (let ((first-child-idx (if (equal (treesit-node-type node) "else_clause")
                                  0
                                1)))
                                        ;skip condition node
         (setq first-child (treesit-node-child node first-child-idx t)
               last-child (treesit-node-child node -1 t)))))
    (when (and first-child last-child)
      (list (treesit-node-start first-child)
            (treesit-node-end last-child)))))

(defun evil-ts-obj-bash-statement-get-sibling (dir node)
  "Implementation of a node fetcher for `evil-ts-obj-conf-sibling-trav'.
Return a next or previous sibling for `NODE' based on value of
`DIR'. This function handles traversing of bash list structure:
`echo h && echo e`. See `evil-ts-obj--get-sibling-bin-op' for
more information. Fallback to `evil-ts-obj--get-sibling-simple',
if NODE is not inside boolean operator. It also stops traversing
when reaching {else,elif}_clause."
  (if-let* ((sibling (evil-ts-obj--get-sibling-bin-op '("list" "binary_expression") dir node)))
      sibling
    (let ((sibling (evil-ts-obj--get-sibling-simple dir node)))
      (pcase (treesit-node-type sibling)
        ((or "else_clause" "elif_clause")
         nil)
        (_ sibling)))))


(defun evil-ts-obj-bash-param-sibling-kind (_cur-node _cur-kind node &optional _sep-regex)
  "Implementation of a kind-func for `evil-ts-obj-conf-sibling-trav'.
Command name is a sibling of its arguments in bash. This function
prevents including command name while traversing over arguments.
For description of CUR-NODE, CUR-KIND, NODE and SEP-REGEX see
`evil-ts-obj-conf-sibling-trav'."
  (when (not (equal (treesit-node-type node) "command_name"))
    'sibling))


(defun evil-ts-obj-bash-ext (spec node)
  "Main extension function for bash.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."

  (pcase spec
    ((pmap (:thing 'compound) (:mod 'inner))
     (evil-ts-obj-bash-extract-compound-inner node))))

(defcustom evil-ts-obj-bash-ext-func
  #'evil-ts-obj-bash-ext
  "Extension function for bash."
  :type 'function
  :group 'evil-ts-obj)


;;;###autoload
(defun evil-ts-obj-bash-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (evil-ts-obj-def-init-lang 'bash evil-ts-obj-bash-things
                             :ext-func evil-ts-obj-bash-ext-func
                             :seps-reg evil-ts-obj-bash-statement-seps
                             :statement-sib-trav (evil-ts-obj-trav-create
                                                  :seps evil-ts-obj-bash-statement-seps-regex
                                                  :fetcher #'evil-ts-obj-bash-statement-get-sibling)
                             :param-sib-trav (evil-ts-obj-trav-create
                                              :kind-func #'evil-ts-obj-bash-param-sibling-kind)))



(provide 'evil-ts-obj-bash)
;;; evil-ts-obj-bash.el ends here
