;;; evil-ts-obj-python.el --- Python setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: ноября 12, 2023
;; Modified: ноября 12, 2023
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
    "raise_statement")
  "Nodes that designate simple statement in python."
  :type '(repeat string)
  :group 'evil-ts-obj)




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

(defvar evil-ts-obj-python-param-parent-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")



(defcustom evil-ts-obj-python-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-compound-nodes))
    (statement ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-statement-nodes))
    (param ,(apply-partially #'evil-ts-obj-common-param-pred
                             evil-ts-obj-python-param-parent-regex)))
  "Things for python."
  :type 'repeate
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-python-param-seps
  ","
  "Separators for python params."
  :type '(choice (repeat string) string)
  :group 'evil-ts-obj)

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
               (cond
                ((equal (treesit-node-type node) "if_statement")
                 (treesit-node-child-by-field-name node "consequence"))
                ((equal (treesit-node-type node) "try_statement")
                 (treesit-node-child-by-field-name node "body"))
                (t
                 (treesit-node-child node -1))))
              ((equal (treesit-node-type block-node) "block")))
    (list (treesit-node-start block-node)
          (treesit-node-end block-node))))



(defun evil-ts-obj-python-ext-func (spec node)
  "Main extension function for python. TODO spec"
  (pcase spec
    ((pmap (:thing 'compound) (:text-obj 'outer) (:op-kind 'mod))
     (evil-ts-obj-python-compound-outer-ext node))
    ((pmap (:thing 'compound) (:text-obj 'inner))
     (evil-ts-obj-python-extract-compound-inner node))
    ((pmap (:thing 'param) (:text-obj 'outer) (:op-kind 'mod))
     (evil-ts-obj-param-outer-mod node evil-ts-obj-python-param-seps))
    ((pmap (:thing 'param) (:text-obj 'upper))
     (evil-ts-obj-param-upper-mod node evil-ts-obj-python-param-seps))
    ((pmap (:thing 'param) (:text-obj 'lower))
     (evil-ts-obj-param-lower-mod node evil-ts-obj-python-param-seps))))

;;;###autoload
(defun evil-ts-obj-python-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (make-local-variable 'treesit-thing-settings)
  (cl-callf append (alist-get 'python treesit-thing-settings)
    evil-ts-obj-python-things)

  (cl-callf plist-put evil-ts-obj-conf-thing-modifiers
   'python #'evil-ts-obj-python-ext-func)

  (cl-callf plist-put evil-ts-obj-conf-sep-regexps 'python
            evil-ts-obj-python-param-seps)

  (cl-callf plist-put evil-ts-obj-conf-nav-things
    'python '(or param statement compound)))



(provide 'evil-ts-obj-python)
;;; evil-ts-obj-python.el ends here
