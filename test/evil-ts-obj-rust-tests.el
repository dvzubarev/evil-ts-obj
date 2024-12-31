;;; evil-ts-obj-cpp-tests.el --- rust tests -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(require 'ert)
(require 'ert-x)
(require 'treesit)
(require 'evil)
(require 'evil-ts-obj)
(require 'evil-ts-obj-rust)

(defun evil-ts-obj-rust-tests-setup ()
  (evil-mode)
  (evil-normal-state)
  (let ((inhibit-message t))
    (rust-ts-mode))
  (indent-tabs-mode -1)
  (evil-ts-obj-mode 1))

(ert-deftest evil-ts-obj-rust-text-objects-test ()
  (skip-unless (treesit-ready-p 'rust))
  (ert-test-erts-file (ert-resource-file "text-objects.erts")))

(ert-deftest evil-ts-obj-rust-movement-test ()
  (skip-unless (treesit-ready-p 'rust))
  (ert-test-erts-file (ert-resource-file "movement.erts")))

(ert-deftest evil-ts-obj-rust-edit-test ()
  (skip-unless (treesit-ready-p 'rust))
  (ert-test-erts-file (ert-resource-file "edit.erts")))


(provide 'evil-ts-obj-rust-tests)
;;; evil-ts-obj-rust-tests.el ends here
