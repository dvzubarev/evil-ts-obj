;;; evil-ts-obj-setup.el --- Setup utils -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-treesit-objects
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides evil text-objects using tree-sitter
;;
;;; Code:


;; languages support
(require 'evil-ts-obj)
(require 'evil-ts-obj-python)

;;;###autoload
(defun evil-ts-obj-setup ()

  (cond
   ((derived-mode-p 'python-ts-mode)
    (evil-ts-obj-python-setup-things)))

  (evil-ts-obj-mode 1))



(provide 'evil-ts-obj-setup)
;;; evil-ts-obj-setup.el ends here
