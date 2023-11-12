;;; evil-ts-obj-python-tests.el --- python tests -*- lexical-binding: t; -*-
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
(require 'evil-ts-obj-python)

(defun evil-ts-obj-python-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (python-ts-mode)
  (evil-ts-obj-python-setup-things))

(ert-deftest evil-ts-obj-python-text-objects-test ()
  (skip-unless (treesit-ready-p 'python))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-python-movement-test ()
  (skip-unless (treesit-ready-p 'python))
  (ert-test-erts-file (ert-resource-file "movement.erts")))


(provide 'evil-ts-obj-python-tests)
;;; evil-ts-obj-python-tests.el ends here
