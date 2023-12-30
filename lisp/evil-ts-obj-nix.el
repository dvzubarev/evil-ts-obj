;;; evil-ts-obj-nix.el --- Nix setting for evil-ts-obj -*- lexical-binding: t; -*-
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
;;  Nix setting for evil-ts-obj
;;
;;; Code:

(require 'treesit)
(require 'evil-ts-obj-conf)
(require 'evil-ts-obj-core)

(defcustom evil-ts-obj-nix-compound-nodes
  '("attrset_expression"
    "rec_attrset_expression")
  "Nodes that designate compound statement in nix."
  :type '(repeat string)
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-nix-statement-nodes
  '("binding"
    "inherit"
    "function_expression"
    "apply_expression"
    "if_expression"
    "let_attrset_expression"
    "let_expression"
    "with_expression")
  "Nodes that designate statement in nix."
  :type '(repeat string)
  :group 'evil-ts-obj)


(defun evil-ts-obj-nix-statement-pred (node)
  "Predicate for detecting statement thing, represented by `NODE'.
For apply_expression and function_expression
it returns t only for the furthest parent of the same type."
  (let ((node-type (treesit-node-type node)))
    (if (member node-type '("apply_expression" "function_expression"))
        (not (equal node-type (treesit-node-type (treesit-node-parent node))))
      t)))

(defvar evil-ts-obj-nix-param-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

(defcustom evil-ts-obj-nix-param-nodes
  '("formal" "ellipses")
  "Nodes that designate parameter in nix."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)


(defvar evil-ts-obj-nix-param-parent-regex nil
  "This variable should be set by `evil-ts-obj-conf-nodes-setter'.")

(defcustom evil-ts-obj-nix-param-parent-nodes
  '("list_expression")
  "Parent nodes for a parameter thing in nix."
  :type '(repeat string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-nodes-setter)

(defun evil-ts-obj-nix-param-pred (node)
  "Predicate for detecting param thing.
Return t if `NODE' is named and it is matching against
`evil-ts-obj-nix-param-regex' or parent is matching against
`evil-ts-obj-nix-param-parent-regex'."
  (when-let (((treesit-node-check node 'named))
             ((not (equal (treesit-node-type node) "comment"))))
    (or (string-match-p evil-ts-obj-nix-param-regex (treesit-node-type node))
        (when-let* ((parent (treesit-node-parent node))
                    (parent-type (treesit-node-type parent)))
          (or (and (equal parent-type "function_expression")
                   (equal (treesit-node-field-name node) "universal"))
              (and (equal parent-type "apply_expression")
                   (equal (treesit-node-field-name node) "argument"))
              (string-match-p evil-ts-obj-nix-param-parent-regex parent-type))))))

(defcustom evil-ts-obj-nix-things
  `((compound ,(evil-ts-obj-conf--make-nodes-regex evil-ts-obj-nix-compound-nodes))
    (statement ,(cons (evil-ts-obj-conf--make-nodes-regex evil-ts-obj-nix-statement-nodes)
                      #'evil-ts-obj-nix-statement-pred))
    (param evil-ts-obj-nix-param-pred))
  "Things for nix."
  :type 'plist
  :group 'evil-ts-obj)

(defvar evil-ts-obj-nix-param-seps-regex nil
  "This variable should be set by `evil-ts-obj-conf-seps-setter'.")

(defcustom evil-ts-obj-nix-param-seps
  '("," ":")
  "Separators for nix params."
  :type '(choice (repeat string) string)
  :group 'evil-ts-obj
  :set #'evil-ts-obj-conf-seps-setter)

(defun evil-ts-obj-nix-extract-compound-inner (node)
  "Return range for a compound inner text object.
Compound is represented by a `NODE'."
  (when-let* ((binding-set-node (treesit-node-child node 1)))
    (list (treesit-node-start binding-set-node)
          (treesit-node-end binding-set-node))))


(defun evil-ts-obj-nix-ext (spec node)
  "Main extension function for nix.
See `evil-ts-obj-conf-thing-modifiers' for details about `SPEC'
and `NODE'."
  (pcase spec
    ((pmap (:thing 'compound) (:text-obj 'inner))
     (evil-ts-obj-nix-extract-compound-inner node))
    ((pmap (:op-kind 'mod) (:thing 'param))
     (evil-ts-obj-common-param-ext spec node evil-ts-obj-nix-param-seps-regex t))))

(defcustom evil-ts-obj-nix-ext-func
  #'evil-ts-obj-nix-ext
  "Extension function for python."
  :type 'function
  :group 'evil-ts-obj)

;;;###autoload
(defun evil-ts-obj-nix-setup-things ()
  "Set all variables needed by evil-ts-obj-core."

  (make-local-variable 'treesit-thing-settings)
  (cl-callf append (alist-get 'nix treesit-thing-settings)
    evil-ts-obj-nix-things)


  (cl-callf plist-put evil-ts-obj-conf-thing-modifiers
   'nix evil-ts-obj-nix-ext-func)

  (cl-callf plist-put evil-ts-obj-conf-sep-regexps 'nix
            evil-ts-obj-nix-param-seps-regex)

  (cl-callf plist-put evil-ts-obj-conf-nav-things
    'nix '(or param statement compound)))



(provide 'evil-ts-obj-nix)
;;; evil-ts-obj-nix.el ends here
