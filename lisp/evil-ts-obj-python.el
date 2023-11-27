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
(require 'evil-ts-obj-common)

(defvar evil-ts-obj-python-compound-nodes
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
  "Nodes that designate compound statement in python.
See `treesit-thing-settings' for more information.")

(defvar evil-ts-obj-python-compound-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-compound-nodes))


(defvar evil-ts-obj-python-statement-nodes
  '("return_statement" "pass_statement" "expression_statement" "named_expression"))

(defvar evil-ts-obj-python-statement-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-statement-nodes))


(defvar evil-ts-obj-python-param-parent-nodes
  '("parameters"
    "lambda_parameters"
    "argument_list"
    "dictionary"
    "list"
    "tuple"))

(defvar evil-ts-obj-python-param-parent-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-python-param-parent-nodes))




(defun evil-ts-obj-python-param-pred (node)
  (when-let* ((named (treesit-node-check node 'named))
              (parent (treesit-node-parent node)))
    (string-match evil-ts-obj-python-param-parent-regex
                  (treesit-node-type parent))))


(defun evil-ts-obj-python-compound-outer-ext (node)
  (when-let* ((is-func (equal (treesit-node-type node) "function_definition"))
              (parent (treesit-node-parent node))
              (is-decorator (equal (treesit-node-type parent) "decorated_definition")))
    (list (treesit-node-start parent)
          (treesit-node-end parent))))

(defun evil-ts-obj-python-extract-compound-inner (node)
  (when-let* ((block-node (if (equal (treesit-node-type node) "if_statement")
                              (treesit-node-child-by-field-name node "consequence")
                            (treesit-node-child node -1)))
              ((equal (treesit-node-type block-node) "block")))
    (list (treesit-node-start block-node)
          (treesit-node-end block-node))))



(defun evil-ts-obj-python-ext-func (spec node)
  "Main extention function for python."
  (pcase spec
    ((pmap (:thing 'compound) (:text-obj 'outer))
     (evil-ts-obj-python-compound-outer-ext node))
    ((pmap (:thing 'compound) (:text-obj 'inner))
     (evil-ts-obj-python-extract-compound-inner node))
    ((pmap (:thing 'param) (:text-obj 'outer) (:op-kind 'mod))
     (evil-ts-obj-common-param-outer-mod node))
    ((pmap (:thing 'param) (:text-obj 'upper))
     (evil-ts-obj-common-param-upper-mod node))
    ((pmap (:thing 'param) (:text-obj 'lower))
     (evil-ts-obj-common-param-lower-mod node))))

;;;###autoload
(defun evil-ts-obj-python-setup-things ()
  (setq-local treesit-thing-settings
              `((python
                 (compound ,evil-ts-obj-python-compound-regex)
                 (statement ,evil-ts-obj-python-statement-regex)
                 (param evil-ts-obj-python-param-pred))))

  (setq-local evil-ts-obj-conf-thing-modifiers
              '(python evil-ts-obj-python-ext-func))


  (setq-local evil-ts-obj-conf-nav-thing
              '(or param statement compound)))



(provide 'evil-ts-obj-python)
;;; evil-ts-obj-python.el ends here
