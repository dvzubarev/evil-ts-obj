;;; evil-ts-obj-avy-tests.el --- avy tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
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
(require 'evil)
(require 'evil-ts-obj-python)
(require 'evil-ts-obj-avy)

(defun evil-ts-obj-avy-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (python-ts-mode)
  (evil-ts-obj-python-setup-things)

  (switch-to-buffer (buffer-name))
  (setq-local tab-width 4)
  (setq-local avy-keys '(?a ?b ?e)))

(ert-deftest evil-ts-obj-avy-text-objects-test ()
  (skip-unless (treesit-ready-p 'python))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-avy-actions-test ()
  (skip-unless (treesit-ready-p 'python))
  (ert-test-erts-file (ert-resource-file "actions.erts")))


(provide 'evil-ts-obj-avy-tests)
;;; evil-ts-obj-avy-tests.el ends here
