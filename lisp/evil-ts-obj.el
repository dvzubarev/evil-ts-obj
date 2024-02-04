;;; evil-ts-obj.el --- Provides evil text-objects using tree-sitter -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-ts-obj
;; Package-Requires: ((emacs "30.0.50") (evil "0") (avy "0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Opinionated set of text objects and movement commands for evil.
;;
;;; Code:

(require 'evil-ts-obj-core)
(require 'evil-ts-obj-edit)

(require 'evil-ts-obj-python)
(require 'evil-ts-obj-bash)
(require 'evil-ts-obj-cpp)

(require 'evil-ts-obj-nix)
(require 'evil-ts-obj-yaml)



;;; minor mode

(defun evil-ts-obj--maybe-create-parser (lang)
  (unless (featurep 'treesit)
    (user-error "Tree-sitter not supported in current Emacs version"))
  (unless (treesit-ready-p lang)
    (user-error (format "%s tree-sitter is not ready!" lang)))
  (treesit-parser-create lang))

(defun evil-ts-obj--guess-lang-from-mode ()
  (pcase major-mode
    ;; exceptions
    ('sh-mode 'bash)
    ((or 'c++-ts-mode 'c++-mode) 'cpp)
    (_
     ;; get first word from mode
     (intern (car (string-split (symbol-name major-mode) "-"))))))

;;;###autoload
(define-minor-mode evil-ts-obj-mode
  "A minor mode with tree sitter keybinds."
  :group 'evil-ts-obj
  :lighter ""

  (when evil-ts-obj-mode
    (let ((lang (evil-ts-obj--guess-lang-from-mode)))
      (evil-ts-obj--maybe-create-parser lang)
      (funcall (intern (format "evil-ts-obj-%s-setup-things" lang))))))


(require 'evil-ts-obj-evil)



(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
