;;; evil-ts-obj-yaml.el --- YAML setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: ноября 12, 2023
;; Modified: ноября 12, 2023
;; Version: 0.0.1
;; Keywords: convenience tools
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  YAML setting for evil-ts-obj
;;
;;; Code:

(require 'treesit)
(require 'evil-ts-obj-conf)

(defvar evil-ts-obj-yaml-compound-nodes
  '("document"
    "block_mapping"
    "block_sequence")
  "Nodes that designate compound statement in yaml.")

(defvar evil-ts-obj-yaml-compound-regex
  (format "^%s$" (regexp-opt evil-ts-obj-yaml-compound-nodes)))


;; (defvar evil-ts-obj-yaml-statement-nodes
;;   '("return_statement" "expression_statement" "named_expression"))

;; (defvar evil-ts-obj-yaml-statement-regex
;;   (format "^%s$" (regexp-opt evil-ts-obj-yaml-statement-nodes)))


(defvar evil-ts-obj-yaml-param-nodes
  '("block_mapping_pair"
    "block_sequence_item"))

(defvar evil-ts-obj-yaml-param-regex
  (format "^%s$" (regexp-opt evil-ts-obj-yaml-param-nodes)))


(defvar evil-ts-obj-yaml-avy-jump-query
  (treesit-query-compile
   'yaml
   (format "[%s]" (concat
                   (mapconcat (lambda (n) (format "(%s) @c " n))
                              evil-ts-obj-yaml-compound-nodes)))))


(defun evil-ts-obj-yaml-param-inner-ext (node)
  (when-let* ((is-list (equal (treesit-node-type node) "block_sequence_item"))
              ;; the 1st child is "-" symbol
              ;; the second child is the list item
              (child (treesit-node-child node 1)))
    (setq node child))
  (list (treesit-node-start node)
        (treesit-node-end node)))

;;;###autoload
(defun evil-ts-obj-yaml-setup-things ()
  (setf (alist-get 'yaml evil-ts-obj-conf-param-inner-ext)
        #'evil-ts-obj-yaml-param-inner-ext)
  (setq-local evil-ts-obj-conf-avy-jump-query evil-ts-obj-yaml-avy-jump-query)
  (setq-local treesit-thing-settings
              `((yaml
                 (compound-around ,evil-ts-obj-yaml-compound-regex)
                 (compound-inner "block_node")
                 (param-inner ,evil-ts-obj-yaml-param-regex))))

  (setq-local evil-ts-obj-conf-nav-dwim-thing
              '(or param-inner compound-around)))



(provide 'evil-ts-obj-yaml)
;;; evil-ts-obj-yaml.el ends here
