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
;; Package-Requires: ((emacs "30.0.50") (evil "0") (avy "0"))
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
(require 'evil-ts-obj-cpp)

(require 'evil-ts-obj-nix)
(require 'evil-ts-obj-yaml)


;; * interactive functions
;; ** Movement

(evil-define-motion evil-ts-obj-next-largest-thing (count)
  "Jump to the next largest thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-largest-thing thing))))

(evil-define-motion evil-ts-obj-same-next-largest-thing (count)
  "Jump to the same next largest thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-largest-thing thing))))

(evil-define-motion evil-ts-obj-previous-largest-thing (count)
  "Jump to the previous largest thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-largest-thing thing))))

(evil-define-motion evil-ts-obj-same-previous-largest-thing (count)
  "Jump to the same previous largest thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-largest-thing thing))))


(evil-define-motion evil-ts-obj-beginning-of-thing (count)
  "Jump to the beginning of the current thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump t
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-begin-of-thing thing))))

(evil-define-motion evil-ts-obj-end-of-thing (count)
  "Jump to the end of the current thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump t
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-end-of-thing thing))))

(evil-define-motion evil-ts-obj-next-thing (count)
  "Jump to the next thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-thing thing))))

(evil-define-motion evil-ts-obj-same-next-thing (count)
  "Jump to the same next thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-thing thing))))

(evil-define-motion evil-ts-obj-previous-thing (count)
  "Jump to the previous thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-thing thing))))

(evil-define-motion evil-ts-obj-same-previous-thing (count)
  "Jump to the same previous thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-thing thing))))

(defmacro evil-ts-obj-define-movement (dir thing)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-%s-%s" dir thing)))
        (impl-name (intern (format "evil-ts-obj--goto-%s-thing"
                                   (pcase dir
                                     ("beginning-of" "begin-of")
                                     ((pred (string-prefix-p "previous"))
                                      (concat "prev" (string-remove-prefix "previous" dir)))
                                     (_ dir))))))
    `(evil-define-motion ,name (count)
       ,(format "Jump to the %s %s." dir thing)
       :type inclusive
       :jump ,(not (null (member dir '("beginning-of" "end-of"))))
       (dotimes (_ (or count 1))
         (,impl-name ',thing)))))

(defmacro evil-ts-obj-setup-all-movement (thing key)
  "Define all movement commands for a `THING'.
Also bind `KEY' to defined commands in all appropriate keymaps."
  `(progn
     ,@(let (result)
         (dolist (move  '("beginning-of"
                          "end-of"
                          "previous"
                          "next"
                          "previous-largest"
                          "next-largest"))
           (let ((map-name (intern (format "evil-ts-obj-goto-%s-map" move)))
                 (command (intern (format "evil-ts-obj-%s-%s" move thing))))
             (push `(evil-ts-obj-define-movement ,move ,thing) result)
             (push `(keymap-set ,map-name (kbd ,key) #',command) result)))
         (nreverse result))))


;; ** Text objects

(defun evil-ts-obj--finalize-text-obj-range (spec range)
  (pcase spec
    ((pmap (:thing 'compound) (:text-obj 'outer))
     (pcase-let ((`(,first-pos ,last-pos) range))
       (when (and
              last-pos
              (eq (char-before last-pos) ?\n))
         (setq last-pos (1- last-pos)))
       (list first-pos last-pos)))
    (_ range)))

(defmacro evil-ts-obj-define-text-obj (thing text-obj)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-%s-%s" thing text-obj))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       ,(format "Select a %s %s object." thing text-obj)
       (let ((spec (evil-ts-obj--make-spec nil ',thing ',text-obj)))
         (evil-ts-obj--finalize-text-obj-range
          spec
          (evil-ts-obj--get-text-obj-range (point) ',thing spec))))))

(defmacro evil-ts-obj-setup-all-text-objects (thing key)
  "Define all text objects for a `THING'.
Also bind `KEY' to defined text objects in all appropriate keymaps."
  `(progn
     ,@(let (result)
         (dolist (to '(outer inner upper lower))
           (let ((map-name (intern (format "evil-ts-obj-%s-text-objects-map" to)))
                 (command (intern (format "evil-ts-obj-%s-%s" thing to))))
             (push `(evil-ts-obj-define-text-obj ,thing ,to) result)
             (push `(keymap-set ,map-name (kbd ,key) #',command) result)))
         (nreverse result))))




;;* default keybindings and minor mode

(defvar evil-ts-obj-inner-text-objects-map (make-sparse-keymap "Inner text objects"))
(defvar evil-ts-obj-outer-text-objects-map (make-sparse-keymap "Outer text objects"))
(defvar evil-ts-obj-upper-text-objects-map (make-sparse-keymap "Upper text objects"))
(defvar evil-ts-obj-lower-text-objects-map (make-sparse-keymap "Lower text objects"))

(evil-ts-obj-setup-all-text-objects compound evil-ts-obj-compound-text-obj-key)
(evil-ts-obj-setup-all-text-objects statement evil-ts-obj-statement-text-obj-key)
(evil-ts-obj-setup-all-text-objects param evil-ts-obj-param-text-obj-key)


(defvar evil-ts-obj-goto-beginning-of-map (make-sparse-keymap "Goto beginning of"))
(defvar evil-ts-obj-goto-end-of-map (make-sparse-keymap "Goto end of"))
(defvar evil-ts-obj-goto-next-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-next-largest-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-largest-map (make-sparse-keymap))

(evil-ts-obj-setup-all-movement compound evil-ts-obj-compound-text-obj-key)
(evil-ts-obj-setup-all-movement statement evil-ts-obj-statement-text-obj-key)
(evil-ts-obj-setup-all-movement param evil-ts-obj-param-text-obj-key)


(defun evil-ts-obj--maybe-create-parser (lang)
  (unless (featurep 'treesit)
    (user-error "Tree-sitter not supported in current Emacs version"))
  (unless (treesit-ready-p lang)
    (user-error (format "%s tree-sitter is not ready!" lang)))
  (treesit-parser-create lang))

(defun evil-ts-obj--guess-lang-from-mode ()
  (pcase major-mode
    ;; exceptions
    ('sh-mode 'bash)
    ((or 'c++-ts-mode 'c++-mode) 'cpp)
    (_
     ;; get first word from mode
     (intern (car (string-split (symbol-name major-mode) "-"))))))

;;;###autoload
(define-minor-mode evil-ts-obj-mode
  "A minor mode with tree sitter keybinds."
  :group 'evil-ts-obj

  (when evil-ts-obj-mode
    (let ((lang (evil-ts-obj--guess-lang-from-mode)))
      (evil-ts-obj--maybe-create-parser lang)
      (funcall (intern (format "evil-ts-obj-%s-setup-things" lang))))))

(defun evil-ts-obj--set-generic-nav-bindings ()
  (evil-define-key 'normal 'evil-ts-obj-mode
    (kbd "M-a") #'evil-ts-obj-beginning-of-thing
    (kbd "M-e") #'evil-ts-obj-end-of-thing
    (kbd "M-n") #'evil-ts-obj-next-largest-thing
    (kbd "C-M-n") #'evil-ts-obj-same-next-largest-thing
    (kbd "M-p") #'evil-ts-obj-previous-largest-thing
    (kbd "C-M-p") #'evil-ts-obj-same-previous-largest-thing
    (kbd "M-f") #'evil-ts-obj-next-thing
    (kbd "C-M-f") #'evil-ts-obj-same-next-thing
    (kbd "M-b") #'evil-ts-obj-previous-thing
    (kbd "C-M-b") #'evil-ts-obj-same-previous-thing))

(defun evil-ts-obj--bind-movement ()
  (pcase-dolist (`(,move . ,key) evil-ts-obj-navigation-keys-prefix)
    (when key
      (evil-define-key '(visual normal) 'evil-ts-obj-mode
        key (symbol-value (intern (format "evil-ts-obj-goto-%s-map" (symbol-name move))))))))

(defun evil-ts-obj--bind-text-objects ()
  (evil-define-key '(visual operator) 'evil-ts-obj-mode
    "i" evil-ts-obj-inner-text-objects-map
    "a" evil-ts-obj-outer-text-objects-map
    "u" evil-ts-obj-upper-text-objects-map
    "o" evil-ts-obj-lower-text-objects-map))

(defun evil-ts-obj--bind-keys ()
  (evil-ts-obj--set-generic-nav-bindings)
  (evil-ts-obj--bind-movement)
  (evil-ts-obj--bind-text-objects)
  (evil-ts-obj-avy--bind-text-objects)
  ;; Without that call keybinding won't activate until a state transition
  ;; see https://github.com/emacs-evil/evil/issues/301
  (evil-normalize-keymaps))

(evil-ts-obj--bind-keys)






(provide 'evil-ts-obj)
;;; evil-ts-obj.el ends here
