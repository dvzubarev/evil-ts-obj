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
  (format "^%s$" (regexp-opt evil-ts-obj-python-compound-nodes)))


(defvar evil-ts-obj-python-statement-nodes
  '("return_statement" "expression_statement" "named_expression"))

(defvar evil-ts-obj-python-statement-regex
  (format "^%s$" (regexp-opt evil-ts-obj-python-statement-nodes)))


(defvar evil-ts-obj-python-param-parent-nodes
  '("parameters"
    "lambda_parameters"
    "argument_list"
    "dictionary"
    "list"
    "tuple"))

(defvar evil-ts-obj-python-param-parent-regex
  (format "^%s$" (regexp-opt evil-ts-obj-python-param-parent-nodes)))


(defvar evil-ts-obj-python-avy-jump-query
  (treesit-query-compile
   'python
   (format "[%s]" (concat
                   (mapconcat (lambda (n) (format "(%s) @c " n))
                              evil-ts-obj-python-compound-nodes)
                   (mapconcat (lambda (n) (format "(%s (_) @p) " n))
                              evil-ts-obj-python-param-parent-nodes)))))

(defun evil-ts-obj-python-inner-pred (node)
  (string-match evil-ts-obj-python-compound-regex
                (treesit-node-type (treesit-node-parent node)))

  ;; (member (treesit-node-type (treesit-node-parent node)) evil-ts-obj-python-compound-nodes)
  )

(defun evil-ts-obj-python-param-pred (node)
  (when-let ((named (treesit-node-check node 'named))
             (parent (treesit-node-parent node)))
    (string-match evil-ts-obj-python-param-parent-regex
                  (treesit-node-type parent))))

(defun evil-ts-obj-python-compound-around-ext (node)
  (when-let* ((is-func (equal (treesit-node-type node) "function_definition"))
              (parent (treesit-node-parent node))
              (is-decorator (equal (treesit-node-type parent) "decorated_definition")))
    (setq node parent))
  (list (treesit-node-start node)
        (treesit-node-end node)))

;;;###autoload
(defun evil-ts-obj-python-setup-things ()
  (setf (alist-get 'python evil-ts-obj-conf-compound-around-ext)
        #'evil-ts-obj-python-compound-around-ext)
  (setq-local evil-ts-obj-conf-avy-jump-query evil-ts-obj-python-avy-jump-query)
  (setq-local treesit-thing-settings
              `((python
                 (compound-around ,evil-ts-obj-python-compound-regex)
                 (compound-inner ,(cons "block" #'evil-ts-obj-python-inner-pred))
                 (statement-around ,evil-ts-obj-python-statement-regex)
                 (param-inner evil-ts-obj-python-param-pred))))

  (setq-local evil-ts-obj-conf-nav-dwim-thing
              '(or param-inner statement-around compound-around)))



(provide 'evil-ts-obj-python)
;;; evil-ts-obj-python.el ends here
