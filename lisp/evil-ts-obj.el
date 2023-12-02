;;; evil-ts-obj.el --- Provides evil text-objects using tree-sitter -*- lexical-binding: t; -*-
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

(require 'evil-ts-obj-core)
(require 'evil-ts-obj-avy)

(require 'evil-ts-obj-python)
(require 'evil-ts-obj-bash)

(require 'evil-ts-obj-yaml)


;; * interactive functions

(evil-define-motion evil-ts-obj-next-largest-thing (count)
  "Jump to the next sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (evil-without-repeat
      (let ((thing (evil-ts-obj--get-nav-thing)))
        (dotimes (_ (or count 1))
          (evil-ts-obj--goto-next-largest-thing thing))))))

(evil-define-motion evil-ts-obj-same-next-largest-thing (count)
  "Jump to the same next sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (evil-without-repeat
      (let ((thing (evil-ts-obj--get-nav-thing t)))
        (dotimes (_ (or count 1))
          (evil-ts-obj--goto-next-largest-thing thing))))))

(evil-define-motion evil-ts-obj-previous-largest-thing (count)
  "Jump to the previous sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-largest-thing thing)))))

(evil-define-motion evil-ts-obj-same-previous-largest-thing (count)
  "Jump to the same previous sibling thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-largest-thing thing)))))


(evil-define-motion evil-ts-obj-beginning-of-thing (count)
  "Jump to the beginning of the current thing."
  :type inclusive
  :jump t
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--begin-of-thing thing)))))

(evil-define-motion evil-ts-obj-end-of-thing (count)
  "Jump to the end of the current thing."
  :type inclusive
  :jump t
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--end-of-thing thing)))))

(evil-define-motion evil-ts-obj-next-thing (count)
  "Jump to the next thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-next-thing thing)))))

(evil-define-motion evil-ts-obj-same-next-thing (count)
  "Jump to the same next thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-next-thing thing)))))

(evil-define-motion evil-ts-obj-previous-thing (count)
  "Jump to the previous thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-thing thing)))))

(evil-define-motion evil-ts-obj-same-previous-thing (count)
  "Jump to the same previous thing."
  :type inclusive
  :jump nil
  (evil-without-repeat
    (let ((thing (evil-ts-obj--get-nav-thing t)))
      (dotimes (_ (or count 1))
        (evil-ts-obj--goto-prev-thing thing)))))





(defmacro evil-ts-obj-define-text-obj (thing text-obj)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-%s-%s" thing text-obj))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       ,(format "Select a %s %s object." thing text-obj)
       (evil-ts-obj--get-text-obj-range (point) ',thing
                                     (evil-ts-obj--make-spec 'mod ',thing ',text-obj)))))

(defun evil-ts-obj--finalize-compound (range)
  (pcase-let ((`(,first-pos ,last-pos) range))
    (when (eq (char-before last-pos) ?\n)
      (setq last-pos (1- last-pos)))
    (list first-pos last-pos)))

(evil-define-text-object evil-ts-obj-compound-outer (count &optional _beg _end _type)
  "Select a compound object."
  (evil-ts-obj--finalize-compound
   (evil-ts-obj--get-text-obj-range (point) 'compound
                                 (evil-ts-obj--make-spec 'mod 'compound 'outer))))

(evil-ts-obj-define-text-obj compound inner)
(evil-ts-obj-define-text-obj compound upper)
(evil-ts-obj-define-text-obj compound lower)


(evil-ts-obj-define-text-obj statement outer)
(evil-ts-obj-define-text-obj statement inner)
(evil-ts-obj-define-text-obj statement upper)
(evil-ts-obj-define-text-obj statement lower)


(evil-ts-obj-define-text-obj param outer)
(evil-ts-obj-define-text-obj param inner)
(evil-ts-obj-define-text-obj param upper)
(evil-ts-obj-define-text-obj param lower)



;;* default keybindings and minor mode

(defvar evil-ts-obj-inner-text-objects-map
  (let ((map (make-sparse-keymap "Inner text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-inner)
    (define-key map (kbd "a") #'evil-ts-obj-param-inner)
    (define-key map (kbd "s") #'evil-ts-obj-statement-inner)
    map))
(defvar evil-ts-obj-outer-text-objects-map
  (let ((map (make-sparse-keymap "Outer text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-outer)
    (define-key map (kbd "a") #'evil-ts-obj-param-outer)
    (define-key map (kbd "s") #'evil-ts-obj-statement-outer)
    map))
(defvar evil-ts-obj-upper-text-objects-map
  (let ((map (make-sparse-keymap "Upper text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-upper)
    (define-key map (kbd "a") #'evil-ts-obj-param-upper)
    (define-key map (kbd "s") #'evil-ts-obj-statement-upper)
    map))
(defvar evil-ts-obj-lower-text-objects-map
  (let ((map (make-sparse-keymap "Lower text objects")))
    (define-key map (kbd "e") #'evil-ts-obj-compound-lower)
    (define-key map (kbd "a") #'evil-ts-obj-param-lower)
    (define-key map (kbd "s") #'evil-ts-obj-statement-lower)
    map))


;;;###autoload
(define-minor-mode evil-ts-obj-mode
  "A minor mode with tree sitter keybinds."
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))


(evil-define-key '(visual operator) evil-ts-obj-mode-map
  "i" evil-ts-obj-inner-text-objects-map
  "a" evil-ts-obj-outer-text-objects-map
  "u" evil-ts-obj-upper-text-objects-map
  "o" evil-ts-obj-lower-text-objects-map)

(evil-define-key 'normal evil-ts-obj-mode-map
  (kbd "M-a") #'evil-ts-obj-beginning-of-thing
  (kbd "M-e") #'evil-ts-obj-end-of-thing
  (kbd "M-n") #'evil-ts-obj-next-largest-thing
  (kbd "C-M-n") #'evil-ts-obj-same-next-largest-thing
  (kbd "M-p") #'evil-ts-obj-previous-largest-thing
  (kbd "C-M-p") #'evil-ts-obj-same-previous-largest-thing
  (kbd "M-f") #'evil-ts-obj-next-thing
  (kbd "C-M-f") #'evil-ts-obj-same-next-thing
  (kbd "M-b") #'evil-ts-obj-previous-thing
  (kbd "C-M-b") #'evil-ts-obj-same-previous-thing)


;;;###autoload
(defun evil-ts-obj-setup ()

  (cond
   ((treesit-parser-list nil 'python)
    (evil-ts-obj-python-setup-things))
   ((treesit-parser-list nil 'bash)
    (evil-ts-obj-bash-setup-things))

   ((treesit-parser-list nil 'yaml)
    (evil-ts-obj-yaml-setup-things)))

  (evil-ts-obj-mode 1))


(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
