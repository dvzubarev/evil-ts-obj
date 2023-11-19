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

(require 'treesit)
(require 'evil-ts-obj-conf)
(require 'evil-ts-obj-common)

(defvar evil-ts-obj-bash-compound-nodes
  '("function_definition"
    "if_statement"
    "elif_clause"
    "else_clause"
    "while_statement"
    "for_statement"
    "c_style_for_statement"
    "case_statement")
  "Nodes that designate compound statement in bash.
See `treesit-thing-settings' for more information.")

(defvar evil-ts-obj-bash-compound-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-bash-compound-nodes))


(defvar evil-ts-obj-bash-statement-nodes
  '("command" "declaration_command" "variable_assignment" "list"))

(defvar evil-ts-obj-bash-statement-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-bash-statement-nodes))



(defvar evil-ts-obj-bash-avy-jump-query
  (treesit-query-compile
   'bash
   (format "[%s]" (concat
                   (mapconcat (lambda (n) (format "(%s) @c " n))
                              evil-ts-obj-bash-compound-nodes)))))


(defun evil-ts-obj-bash-param-pred (node)
  (equal (treesit-node-field-name node) "argument"))

(defun evil-ts-obj-bash-extract-compound-inner (node)
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
       (setq first-child (treesit-node-child node 1 t) )
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

(defun evil-ts-obj-bash-statement-ext (node)
  (when-let* ((next-sibl (treesit-node-next-sibling node))
              ((equal (treesit-node-type next-sibl) ";")))
    (list (treesit-node-start node)
          (treesit-node-end next-sibl))))

;;;###autoload
(defun evil-ts-obj-bash-setup-things ()
  (setq-local evil-ts-obj-conf-avy-jump-query evil-ts-obj-bash-avy-jump-query)

  (setq-local treesit-thing-settings
              `((bash
                 (compound ,evil-ts-obj-bash-compound-regex)
                 (statement ,evil-ts-obj-bash-statement-regex)
                 (param evil-ts-obj-bash-param-pred))))
  (setq-local evil-ts-obj-conf-thing-modifiers
              '(bash
                (
                 compound ((:scope inner
                            :func evil-ts-obj-bash-extract-compound-inner))
                 statement ((:scope around
                             :func evil-ts-obj-bash-statement-ext))
                 param ((:scope around
                         :func evil-ts-obj-common-param-around-mod)))))

  (setq-local evil-ts-obj-conf-nav-dwim-thing
              '(or param statement compound)))



(provide 'evil-ts-obj-bash)
;;; evil-ts-obj-bash.el ends here
