;;; evil-ts-obj-cpp-tests.el --- cpp tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Created: November 12, 2023
;; Modified: November 12, 2023
;; Version: 0.0.1
;; Keywords: tools convenience
;; Homepage: https://github.com/dvzubarev/evil-ts-obj
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
(require 'evil-ts-obj-cpp)

(defun evil-ts-obj-cpp-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (let ((inhibit-message t))
    (c++-ts-mode))
  (indent-tabs-mode -1)
  (evil-ts-obj-mode 1))

(ert-deftest evil-ts-obj-cpp-text-objects-test ()
  (skip-unless (treesit-ready-p 'cpp))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-cpp-movement-test ()
  (skip-unless (treesit-ready-p 'cpp))
  (ert-test-erts-file (ert-resource-file "movement.erts")))

(ert-deftest evil-ts-obj-cpp-edit-test ()
  (skip-unless (treesit-ready-p 'cpp))
  (ert-test-erts-file (ert-resource-file "edit.erts")))




(provide 'evil-ts-obj-cpp-tests)
;;; evil-ts-obj-cpp-tests.el ends here
