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

(require 'evil-ts-obj-def)

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
  :type 'plist
  :group 'evil-ts-obj)

(defun evil-ts-obj-yaml-param-mod (node)
  (when-let* ((is-list (equal (treesit-node-type node) "block_sequence_item"))
              ;; the 1st child is "-" symbol
              ;; the second child is the list item
              (child (treesit-node-child node 1)))
    (list (treesit-node-start child)
          (treesit-node-end child))))

(defun evil-ts-obj-yaml-ext (spec node)
  "Main extension function for yaml.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."

  (pcase spec
    ((pmap (:thing 'param) (:mod 'inner) (:act 'op)
           (:command (pred (not (eq 'evil-ts-obj-raise-dwim)))))
     (evil-ts-obj-yaml-param-mod node))))

(defcustom evil-ts-obj-yaml-ext-func
  #'evil-ts-obj-yaml-ext
  "Extension function for yaml."
  :type 'function
  :group 'evil-ts-obj)

;;;###autoload
(defun evil-ts-obj-yaml-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (evil-ts-obj-def-init-conf-lang 'yaml evil-ts-obj-yaml-things
                                  :ext-func evil-ts-obj-yaml-ext-func))



(provide 'evil-ts-obj-yaml)
;;; evil-ts-obj-yaml.el ends here
