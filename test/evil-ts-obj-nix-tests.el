;;; evil-ts-obj-nix-tests.el --- nix tests -*- lexical-binding: t; -*-
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
(require 'evil-ts-obj)
(require 'evil-ts-obj-nix)

(defun evil-ts-obj-nix-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (nix-ts-mode)
  (evil-ts-obj-mode 1))

(ert-deftest evil-ts-obj-nix-text-objects-test ()
 (skip-unless (treesit-ready-p 'nix))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-nix-movement-test ()
  (skip-unless (treesit-ready-p 'nix))
  (ert-test-erts-file (ert-resource-file "movement.erts")))




(provide 'evil-ts-obj-nix-tests)
;;; evil-ts-obj-nix-tests.el ends here
