;;; evil-ts-obj-yaml-tests.el --- yaml tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
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
(require 'evil-ts-obj)

(defun evil-ts-obj-yaml-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (yaml-ts-mode)
  (evil-ts-obj-mode 1))

(ert-deftest evil-ts-obj-yaml-text-objects-test ()
  (skip-unless (treesit-ready-p 'yaml))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-yaml-movement-test ()
  (skip-unless (treesit-ready-p 'yaml))
  (ert-test-erts-file (ert-resource-file "movement.erts")))

(ert-deftest evil-ts-obj-yaml-edit-test ()
  (skip-unless (treesit-ready-p 'yaml))
  (ert-test-erts-file (ert-resource-file "edit.erts")))

(provide 'evil-ts-obj-yaml-tests)
;;; evil-ts-obj-yaml-tests.el ends here
