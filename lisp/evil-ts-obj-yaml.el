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
(require 'evil-ts-obj-core)

(defcustom evil-ts-obj-yaml-compound-nodes
  '("document"
    "block_mapping"
    "block_sequence")
  "Nodes that designate compound statement in yaml."
  :type '(repeat string)
  :group 'evil-ts-obj)




(defcustom evil-ts-obj-yaml-param-nodes
  '("block_mapping_pair"
    "block_sequence_item")
  "Nodes that designate parameter in yaml."
  :type '(repeat string)
  :group 'evil-ts-obj)



(defcustom evil-ts-obj-yaml-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-yaml-compound-nodes))
    (param ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-yaml-param-nodes)))
  "Things for yaml."
  :type 'repeate
  :group 'evil-ts-obj)

(defun evil-ts-obj-yaml-param-mod (node)
  (when-let* ((is-list (equal (treesit-node-type node) "block_sequence_item"))
              ;; the 1st child is "-" symbol
              ;; the second child is the list item
              (child (treesit-node-child node 1)))
    (list (treesit-node-start child)
          (treesit-node-end child))))

(defun evil-ts-obj-yaml-ext-func (spec node)
  "Main extension function for yaml."

  (pcase spec
    ((pmap (:thing 'param) (:text-obj 'inner))
     (evil-ts-obj-yaml-param-mod node))
    ((pmap (:thing 'param) (:op-kind 'nav))
     (evil-ts-obj-yaml-param-mod node))
    ((pmap (:thing 'param) (:text-obj 'outer))
     (evil-ts-obj-param-outer-mod node))))

;;;###autoload
(defun evil-ts-obj-yaml-setup-things ()
  (cl-callf append (alist-get 'yaml treesit-thing-settings)
    evil-ts-obj-yaml-things)

  (setq-local evil-ts-obj-conf-thing-modifiers
              '(yaml evil-ts-obj-yaml-ext-func))


  (setq-local evil-ts-obj-conf-nav-thing
              '(or param compound)))



(provide 'evil-ts-obj-yaml)
;;; evil-ts-obj-yaml.el ends here
