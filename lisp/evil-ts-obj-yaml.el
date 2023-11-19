;;; evil-ts-obj-yaml.el --- YAML setting for evil-ts-obj -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
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
(require 'evil-ts-obj-common)

(defvar evil-ts-obj-yaml-compound-nodes
  '("document"
    "block_mapping"
    "block_sequence")
  "Nodes that designate compound statement in yaml.")

(defvar evil-ts-obj-yaml-compound-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-yaml-compound-nodes))


(defvar evil-ts-obj-yaml-param-nodes
  '("block_mapping_pair"
    "block_sequence_item"))

(defvar evil-ts-obj-yaml-param-regex
  (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-yaml-param-nodes))


(defvar evil-ts-obj-yaml-avy-jump-query
  (treesit-query-compile
   'yaml
   (format "[%s]" (concat
                   (mapconcat (lambda (n) (format "(%s) @c " n))
                              evil-ts-obj-yaml-compound-nodes)))))

(defun evil-ts-obj-yaml-param-mod (node)
  (when-let* ((is-list (equal (treesit-node-type node) "block_sequence_item"))
              ;; the 1st child is "-" symbol
              ;; the second child is the list item
              (child (treesit-node-child node 1)))
    (list (treesit-node-start child)
          (treesit-node-end child))))

;;;###autoload
(defun evil-ts-obj-yaml-setup-things ()
  (setq-local treesit-thing-settings
              `((yaml
                 (compound ,evil-ts-obj-yaml-compound-regex)
                 (param ,evil-ts-obj-yaml-param-regex))))

  (setq-local evil-ts-obj-conf-thing-modifiers
              '(yaml
                (param ((:scope (inner nav)
                         :func evil-ts-obj-yaml-param-mod)
                        (:scope around
                         :func evil-ts-obj-common-param-around-mod)))))

  (setq-local evil-ts-obj-conf-avy-jump-query evil-ts-obj-yaml-avy-jump-query)


  (setq-local evil-ts-obj-conf-nav-dwim-thing
              '(or param compound)))



(provide 'evil-ts-obj-yaml)
;;; evil-ts-obj-yaml.el ends here
