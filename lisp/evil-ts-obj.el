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
;; Homepage: https://github.com/dvzubarev/evil-ts-obj
;; Package-Requires: ((emacs "30.0.50") (evil "0") (avy "0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Opinionated set of text objects and movement commands for evil.
;;
;;; Code:

(require 'evil-ts-obj-core)
(require 'evil-ts-obj-avy)
(require 'evil-ts-obj-edit)

(require 'evil-ts-obj-python)
(require 'evil-ts-obj-bash)
(require 'evil-ts-obj-cpp)

(require 'evil-ts-obj-nix)
(require 'evil-ts-obj-yaml)


;;;  interactive functions
;;;; Movement

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
  "Jump to the beginning of the current thing from `evil-ts-obj-conf-nav-things'.
When the point is already at the beginning, move to the beginning
of the parent thing."
  :type inclusive
  :jump t
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-begin-of-thing thing))))

(evil-define-motion evil-ts-obj-end-of-thing (count)
  "Jump to the end of the current thing from `evil-ts-obj-conf-nav-things'.
When the point is already at the end, move to the end of the
parent thing."
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


;;;; Text objects

(defmacro evil-ts-obj-define-text-obj (thing mod)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-%s-%s" thing mod))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       ,(format "Select a %s %s text object." thing mod)
       (let ((spec (evil-ts-obj--make-spec ',thing 'op ',mod)))
         (evil-ts-obj--get-text-obj-range (point) ',thing spec)))))

(defmacro evil-ts-obj-setup-all-text-objects (thing key)
  "Define all text objects for a `THING'.
Also bind `KEY' to defined text objects in all appropriate keymaps."
  `(progn
     ,@(let (result)
         (dolist (mod '(outer inner upper lower))
           (let ((map-name (intern (format "evil-ts-obj-%s-text-objects-map" mod)))
                 (command (intern (format "evil-ts-obj-%s-%s" thing mod))))
             (push `(evil-ts-obj-define-text-obj ,thing ,mod) result)
             (push `(keymap-set ,map-name (kbd ,key) #',command) result)))
         (nreverse result))))

(evil-define-text-object evil-ts-obj-last-text-obj (count &optional _beg _end _type)
  (evil-ts-obj-last-range))

;;;; Operators


(evil-define-operator evil-ts-obj-replace (beg end type)
  "Replace content of one region with the content of another one."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (evil-ts-obj-edit--replace-operator beg end))

(evil-define-operator evil-ts-obj-swap (beg end type)
  "Swap content of two regions."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (evil-ts-obj-edit--swap-operator beg end))

(evil-define-operator evil-ts-obj-clone-after (beg end type)
  "Copy content of range to the position after END."
  :move-point nil
  (evil-ts-obj-edit--clone-after-operator beg end))

(evil-define-operator evil-ts-obj-teleport-after (beg end type)
  "Move content of range to the position after END."
  :move-point nil
  (evil-ts-obj-edit--teleport-after-operator beg end))

(evil-define-operator evil-ts-obj-clone-after-dwim ()
  (interactive)
  (evil-ts-obj-edit--clone-dwim-impl t))

(evil-define-operator evil-ts-obj-clone-before (beg end type)
  "Copy content of range before BEG."
  :move-point nil
  (evil-ts-obj-edit--clone-before-operator beg end))

(evil-define-operator evil-ts-obj-clone-before-dwim ()
  (interactive)
  (evil-ts-obj-edit--clone-dwim-impl nil))

(evil-define-operator evil-ts-obj-raise (beg end type count)
  "Replace parent thing with the specified range."
  :move-point nil
  :repeat t
  (interactive "<R><c>")
  (evil-ts-obj-edit--raise-operator beg end count))

(evil-define-operator evil-ts-obj-raise-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--raise-dwim count))

(evil-define-operator evil-ts-obj-drag-up (count)
  (interactive "<c>")
  (evil-ts-obj-edit--drag 'prev count))

(evil-define-operator evil-ts-obj-drag-down (count)
  (interactive "<c>")
  (evil-ts-obj-edit--drag 'next count))

(evil-define-operator evil-ts-obj-extract-up (beg end type count)
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj-edit--extract-operator-impl beg end count))

(evil-define-operator evil-ts-obj-extract-up-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--extract-dwim-impl count))

(evil-define-operator evil-ts-obj-extract-down (beg end type count)
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj-edit--extract-operator-impl beg end count t))

(evil-define-operator evil-ts-obj-extract-down-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--extract-dwim-impl count t))

(evil-define-operator evil-ts-obj-inject-up (beg end type count)
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj-edit--inject-operator-impl beg end count t))

(evil-define-operator evil-ts-obj-inject-up-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--inject-dwim-impl count t))

(evil-define-operator evil-ts-obj-inject-down (beg end type count)
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj-edit--inject-operator-impl beg end count))

(evil-define-operator evil-ts-obj-inject-down-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--inject-dwim-impl count))

(evil-define-operator evil-ts-obj-slurp (count)
  (interactive "<c>")
  (evil-ts-obj-edit--slurp count))

(evil-define-operator evil-ts-obj-barf (count)
  (interactive "<c>")
  (evil-ts-obj-edit--barf count))


;;; default keybindings and minor mode

(defvar evil-ts-obj-inner-text-objects-map (make-sparse-keymap "Inner text objects"))
(defvar evil-ts-obj-outer-text-objects-map (make-sparse-keymap "Outer text objects"))
(defvar evil-ts-obj-upper-text-objects-map (make-sparse-keymap "Upper text objects"))
(defvar evil-ts-obj-lower-text-objects-map (make-sparse-keymap "Lower text objects"))

(evil-ts-obj-setup-all-text-objects compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-setup-all-text-objects statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-setup-all-text-objects param evil-ts-obj-param-thing-key)


(defvar evil-ts-obj-goto-beginning-of-map (make-sparse-keymap "Goto beginning of"))
(defvar evil-ts-obj-goto-end-of-map (make-sparse-keymap "Goto end of"))
(defvar evil-ts-obj-goto-next-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-next-largest-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-largest-map (make-sparse-keymap))

(evil-ts-obj-setup-all-movement compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-setup-all-movement statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-setup-all-movement param evil-ts-obj-param-thing-key)


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
  :lighter ""

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
