;;; evil-ts-obj-conf.el --- Conf utils -*- lexical-binding: t; -*-
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


(defvar-local evil-ts-obj-conf-compound-around-ext nil)
(defvar-local evil-ts-obj-conf-compound-inner-ext nil)
(defvar-local evil-ts-obj-conf-param-inner-ext nil)

(defvar-local evil-ts-obj-conf-avy-jump-query nil)
(defvar-local evil-ts-obj-conf-nav-thing nil)
(defvar-local evil-ts-obj-conf-nav-dwim-thing 'compound-around)



(provide 'evil-ts-obj-conf)
;;; evil-ts-obj-conf.el ends here
