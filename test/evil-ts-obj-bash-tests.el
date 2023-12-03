;;; evil-ts-obj-bash-tests.el --- bash tests -*- lexical-binding: t; -*-
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
;;
;;
;;; Code:


(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'evil)
(require 'evil-ts-obj-bash)

(defun evil-ts-obj-bash-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (let ((inhibit-message t))
    (bash-ts-mode))
  (evil-ts-obj-bash-setup-things))

(ert-deftest evil-ts-obj-bash-text-objects-test ()
  (skip-unless (treesit-ready-p 'bash))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))


(provide 'evil-ts-obj-bash-tests)
;;; evil-ts-obj-bash-tests.el ends here
