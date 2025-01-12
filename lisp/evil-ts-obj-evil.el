;;; evil-ts-obj-;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>evil.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>

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


(require 'evil)

(require 'evil-ts-obj-avy)
(require 'evil-ts-obj-edit)

(defcustom evil-ts-obj-compound-thing-key "e"
  "Default key binding for compound text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-statement-thing-key "s"
  "Default key binding for statement text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-param-thing-key "a"
  "Default key binding for param text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-string-thing-key "q"
  "Default key binding for string text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-navigation-keys-prefix
  '((beginning-of . "(")
    (end-of . ")")
    (previous . "[")
    (next . "]")
    (previous-sibling . "{")
    (next-sibling . "}"))
  "Default bindings for movement commands."
  :type 'alist
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-avy-key-prefix "z"
  "Default key binding for avy text objects."
  :type 'string
  :group 'evil-ts-obj)

(defcustom evil-ts-obj-enabled-keybindings
  '(generic-navigation navigation text-objects edit-operators avy)
  "List of keybindings that should be enabled by default."
  :type '(repeat symbol)
  :group 'evil-ts-obj)

;;; Avy integration

(defvar evil-ts-obj-avy--activate-motion-range-advice nil)

(defun evil-ts-obj-avy--evil-motion-advice (&rest _r)
  "This advice prevents evil-motion-range from restoring point after
motion is executed.
It is needed so evil operators can work on
text objects in in other windows."

  (when evil-ts-obj-avy--activate-motion-range-advice
    (setq evil-ts-obj-avy--activate-motion-range-advice nil
          evil-inhibit-operator nil)))

(advice-add 'evil-motion-range :after #'evil-ts-obj-avy--evil-motion-advice)

(defmacro evil-ts-obj-avy-define-text-obj (thing mod)
  (declare (indent defun))
  (let ((name (intern (format "evil-ts-obj-avy-%s-%s-text-obj" thing mod))))
    `(evil-define-text-object ,name (count &optional _beg _end _type)
       (when evil-this-operator
         (setq evil-ts-obj-avy--activate-motion-range-advice t
               evil-inhibit-operator t))
       (if-let ((range (evil-ts-obj-avy-act-on-text-obj ',thing ',mod)))
           range
         ;; Return an empty range so evil-motion-range doesn't try to guess
         (let ((p (point)))
           (list p p 'exclusive))))))

(defmacro evil-ts-obj-avy-setup-all-text-objects (thing key)
  "Define all text objects for a `THING'.
Also bind `KEY' to defined text objects in all appropriate keymaps."
  `(progn
     ,@(let (result)
         (dolist (mod '(outer inner upper UPPER lower LOWER))
           (let ((map-name (intern (format "evil-ts-obj-avy-%s-text-objects-map" mod)))
                 (command (intern (format "evil-ts-obj-avy-%s-%s-text-obj" thing mod))))
             (push `(evil-ts-obj-avy-define-text-obj ,thing ,mod) result)
             (push `(keymap-set ,map-name (kbd ,key) #',command) result)))
         (nreverse result))))


(evil-ts-obj-avy-setup-all-text-objects compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-avy-setup-all-text-objects statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-avy-setup-all-text-objects param evil-ts-obj-param-thing-key)
(evil-ts-obj-avy-setup-all-text-objects str evil-ts-obj-string-thing-key)


(evil-ts-obj-avy-define-all-paste-cmds compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-avy-define-all-paste-cmds statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-avy-define-all-paste-cmds param evil-ts-obj-param-thing-key)
(evil-ts-obj-avy-define-all-paste-cmds str evil-ts-obj-string-thing-key)

;;;  interactive functions
;;;; Movement

(evil-define-motion evil-ts-obj-next-sibling (count)
  "Jump to the next sibling thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-sibling-thing thing))))

(evil-define-motion evil-ts-obj-same-next-sibling (count)
  "Jump to the next sibling of the same type as the current thing."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-next-sibling-thing thing))))

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

(evil-define-motion evil-ts-obj-previous-sibling (count)
  "Jump to the previous sibling thing from `evil-ts-obj-conf-nav-things'."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-sibling-thing thing))))

(evil-define-motion evil-ts-obj-same-previous-sibling (count)
  "Jump to the previous sibling of the same type as the current thing."
  :type inclusive
  :jump nil
  (let ((thing (evil-ts-obj--get-nav-thing t)))
    (dotimes (_ (or count 1))
      (evil-ts-obj--goto-prev-sibling-thing thing))))

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
                          "previous-sibling"
                          "next-sibling"))
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
         (dolist (mod '(outer inner upper UPPER lower LOWER))
           (let ((map-name (intern (format "evil-ts-obj-%s-text-objects-map" mod)))
                 (command (intern (format "evil-ts-obj-%s-%s" thing mod))))
             (push `(evil-ts-obj-define-text-obj ,thing ,mod) result)
             (push `(keymap-set ,map-name (kbd ,key) #',command) result)))
         (nreverse result))))

(evil-define-text-object evil-ts-obj-last-text-obj (count &optional _beg _end _type)
  (evil-ts-obj-last-range))

;;;; Operators

;;;;; operator helpers

(defmacro evil-ts-obj--adjust-linewise-range (beg end type)
  "Make evil line-wise ranges to be compatible with the core library.
Core can handle multiline strings and paste it properly in new
locations preserving original indentation. But we have to modify
line-wise range so it can be properly handled by the core.
Namely, delete leading spaces and a trailing newline. BEG END and
TYPE are arguments from evil operator."
  `(when (eq ,type 'line)
     (setq ,beg
           (save-excursion
             (goto-char ,beg)
             (skip-chars-forward " \t")
             (point))
           ,end (if (eq (char-before ,end) ?\n)
                    (1- ,end)
                  ,end))))


;;;;; operator definitions

(evil-define-operator evil-ts-obj-replace (beg end type)
  "Replace content of one region with the content of another one."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--replace-operator beg end))

(evil-define-operator evil-ts-obj-swap (beg end type)
  "Swap content of two regions."
  :move-point nil
  :repeat t
  (interactive "<R>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--swap-operator beg end))

(evil-define-operator evil-ts-obj-clone-after (beg end type arg)
  "Copy content of range to the position after END."
  :move-point nil
  (interactive "<R>p")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--clone-after-operator beg end (eql arg 4)))

(evil-define-operator evil-ts-obj-teleport-after (beg end type)
  "Move content of range to the position after END."
  :move-point nil
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--teleport-after-operator beg end))

(evil-define-operator evil-ts-obj-clone-after-dwim (arg)
  "Clone current text object at point and paste it after the current one."
  (interactive "p")
  (evil-ts-obj-edit--clone-dwim-impl t (eql arg 4)))

(evil-define-operator evil-ts-obj-clone-before (beg end type arg)
  "Copy content of range before BEG."
  :move-point nil
  (interactive "<R>p")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--clone-before-operator beg end (eql arg 4)))

(evil-define-operator evil-ts-obj-teleport-before (beg end type)
  "Move content of range to the position before BEG."
  :move-point nil
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--teleport-after-operator beg end))

(evil-define-operator evil-ts-obj-clone-before-dwim (arg)
  "Clone current text object at point and paste it before the current one."
  (interactive "p")
  (evil-ts-obj-edit--clone-dwim-impl nil (eql arg 4)))

(evil-define-operator evil-ts-obj-raise (beg end type count)
  "Replace parent thing with the specified range."
  :move-point nil
  :repeat t
  (interactive "<R><c>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--raise-operator beg end count))

(evil-define-operator evil-ts-obj-raise-dwim (count)
  (interactive "<c>")
  (evil-ts-obj-edit--raise-dwim count))

(evil-define-operator evil-ts-obj-swap-dwim-up (count)
  "Swap a current text object with the previous sibling.
When COUNT is greater then 1, swap current text object with the
Nth sibling."

  (interactive "<c>")
  (evil-ts-obj-edit--swap-dwim 'prev count))

(evil-define-operator evil-ts-obj-swap-dwim-down (count)
  "Swap a current text object with the next sibling.
When COUNT is greater then 1, swap current text object with the
Nth sibling."
  (interactive "<c>")
  (evil-ts-obj-edit--swap-dwim 'next count))

(evil-define-operator evil-ts-obj-extract-up (beg end type count)
  "Teleport text from BEG END range before parent text object.
Parent text object is determined by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--extract-operator-impl beg end count))

(evil-define-operator evil-ts-obj-extract-up-dwim (count)
  "Teleport current text object before parent text object.
Current and parent text object are determined by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  (interactive "<c>")
  (evil-ts-obj-edit--extract-dwim-impl count))

(evil-define-operator evil-ts-obj-extract-down (beg end type count)
  "Teleport text from BEG END range after parent text object.
Parent text object is determi by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--extract-operator-impl beg end count t))

(evil-define-operator evil-ts-obj-extract-down-dwim (count)
  "Teleport current text object after parent text object.
Current and parent text object are determined by the
`evil-ts-obj-conf-extract-rules' variable. When COUNT is set
select Nth parent."
  (interactive "<c>")
  (evil-ts-obj-edit--extract-dwim-impl count t))

(evil-define-operator evil-ts-obj-inject-up (beg end type count)
  "Teleport text from BEG END range inside previous text object.
Previous text object is determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--inject-operator-impl beg end count t))

(evil-define-operator evil-ts-obj-inject-up-dwim (count)
  "Teleport current text object inside previous text object.
Previous text object is determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  (interactive "<c>")
  (evil-ts-obj-edit--inject-dwim-impl count t))

(evil-define-operator evil-ts-obj-inject-down (beg end type count)
  "Teleport text from BEG END range inside next text object.
Previous text object is determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  :move-point nil
  (interactive "<R><c>")
  (evil-ts-obj--adjust-linewise-range beg end type)
  (evil-ts-obj-edit--inject-operator-impl beg end count))

(evil-define-operator evil-ts-obj-inject-down-dwim (count)
  "Teleport current text object inside next text object.
Next text object is determined by the
`evil-ts-obj-conf-inject-rules' variable. Usually inner compounds
are used as place for injection. When COUNT is set select N-1th
child of next/previous text object."
  (interactive "<c>")
  (evil-ts-obj-edit--inject-dwim-impl count))

(evil-define-operator evil-ts-obj-slurp (count)
  "Extend current compound with sibling statements COUNT times.
When point is inside the compound or at the end of the compound
slurp lower statements. If point is at the beginning slurp upper
statements."
  (interactive "<c>")
  (evil-ts-obj-edit--slurp count))

(evil-define-operator evil-ts-obj-barf (count)
  "Shrink current compound extracting inner statements COUNT times.
When point is inside the compound or at the end of the compound
barf bottommost statements. If point is at the beginning barf
topmost statments."
  (interactive "<c>")
  (evil-ts-obj-edit--barf count))

(evil-define-operator evil-ts-obj-convolute (count)
  "Swap parent node with the grandparent node for the current text node."
  (interactive "<c>")
  (evil-ts-obj-edit--convolute count))


;;; default keybindings

(defvar evil-ts-obj-inner-text-objects-map (make-sparse-keymap "Inner text objects"))
(defvar evil-ts-obj-outer-text-objects-map (make-sparse-keymap "Outer text objects"))
(defvar evil-ts-obj-upper-text-objects-map (make-sparse-keymap "Upper text objects"))
(defvar evil-ts-obj-UPPER-text-objects-map (make-sparse-keymap "UPPER text objects"))
(defvar evil-ts-obj-lower-text-objects-map (make-sparse-keymap "Lower text objects"))
(defvar evil-ts-obj-LOWER-text-objects-map (make-sparse-keymap "LOWER text objects"))

(evil-ts-obj-setup-all-text-objects compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-setup-all-text-objects statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-setup-all-text-objects param evil-ts-obj-param-thing-key)
(evil-ts-obj-setup-all-text-objects str evil-ts-obj-string-thing-key)


(defvar evil-ts-obj-goto-beginning-of-map (make-sparse-keymap "Goto beginning of"))
(defvar evil-ts-obj-goto-end-of-map (make-sparse-keymap "Goto end of"))
(defvar evil-ts-obj-goto-next-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-next-sibling-map (make-sparse-keymap))
(defvar evil-ts-obj-goto-previous-sibling-map (make-sparse-keymap))

(evil-ts-obj-setup-all-movement compound evil-ts-obj-compound-thing-key)
(evil-ts-obj-setup-all-movement statement evil-ts-obj-statement-thing-key)
(evil-ts-obj-setup-all-movement param evil-ts-obj-param-thing-key)
(evil-ts-obj-setup-all-movement str evil-ts-obj-string-thing-key)


(defun evil-ts-obj--set-generic-nav-bindings ()

  (evil-define-key 'normal 'evil-ts-obj-mode
    (kbd "M-a") #'evil-ts-obj-beginning-of-thing
    (kbd "M-e") #'evil-ts-obj-end-of-thing
    (kbd "M-n") #'evil-ts-obj-next-sibling
    (kbd "C-M-n") #'evil-ts-obj-same-next-sibling
    (kbd "M-p") #'evil-ts-obj-previous-sibling
    (kbd "C-M-p") #'evil-ts-obj-same-previous-sibling
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
    "U" evil-ts-obj-UPPER-text-objects-map
    "o" evil-ts-obj-lower-text-objects-map
    "O" evil-ts-obj-LOWER-text-objects-map))

(defun evil-ts-obj--bind-edit-keys ()
  (evil-define-key '(visual normal) 'evil-ts-obj-mode
    "zx" #'evil-ts-obj-swap
    "zR" #'evil-ts-obj-replace
    "zr" #'evil-ts-obj-raise
    "zc" #'evil-ts-obj-clone-after
    "zC" #'evil-ts-obj-clone-before
    "zt" #'evil-ts-obj-teleport-after
    "zT" #'evil-ts-obj-teleport-before
    "zE" #'evil-ts-obj-extract-up
    "ze" #'evil-ts-obj-extract-down
    "zs" #'evil-ts-obj-inject-down
    "zS" #'evil-ts-obj-inject-up)
  (evil-define-key 'normal 'evil-ts-obj-mode
    (kbd "M-r") #'evil-ts-obj-raise-dwim
    (kbd "M-J") #'evil-ts-obj-swap-dwim-down
    (kbd "M-K") #'evil-ts-obj-swap-dwim-up
    (kbd "M-c") #'evil-ts-obj-clone-after-dwim
    (kbd "M-C") #'evil-ts-obj-clone-before-dwim
    (kbd "M-h") #'evil-ts-obj-extract-up-dwim
    (kbd "M-l") #'evil-ts-obj-extract-down-dwim
    (kbd "M-s") #'evil-ts-obj-inject-down-dwim
    (kbd "M-S") #'evil-ts-obj-inject-up-dwim
    (kbd "M->") #'evil-ts-obj-slurp
    (kbd "M-<") #'evil-ts-obj-barf))

(defun evil-ts-obj--avy-bind-text-objects ()
  (evil-define-key '(normal operator visual) 'evil-ts-obj-mode
    (kbd (concat evil-ts-obj-avy-key-prefix " i")) evil-ts-obj-avy-inner-text-objects-map
    (kbd (concat evil-ts-obj-avy-key-prefix " a")) evil-ts-obj-avy-outer-text-objects-map
    (kbd (concat evil-ts-obj-avy-key-prefix " u")) evil-ts-obj-avy-upper-text-objects-map
    (kbd (concat evil-ts-obj-avy-key-prefix " U")) evil-ts-obj-avy-UPPER-text-objects-map
    (kbd (concat evil-ts-obj-avy-key-prefix " o")) evil-ts-obj-avy-lower-text-objects-map
    (kbd (concat evil-ts-obj-avy-key-prefix " O")) evil-ts-obj-avy-LOWER-text-objects-map))

(defun evil-ts-obj--bind-keys ()
  (when (memq 'generic-navigation evil-ts-obj-enabled-keybindings)
    (evil-ts-obj--set-generic-nav-bindings))
  (when (memq 'navigation evil-ts-obj-enabled-keybindings)
    (evil-ts-obj--bind-movement))
  (when (memq 'text-objects evil-ts-obj-enabled-keybindings)
    (evil-ts-obj--bind-text-objects))
  (when (memq 'edit-operators evil-ts-obj-enabled-keybindings)
    (evil-ts-obj--bind-edit-keys))
  (when (memq 'avy evil-ts-obj-enabled-keybindings)
    (evil-ts-obj--avy-bind-text-objects))
  ;; Without that call keybinding won't activate until a state transition
  ;; see https://github.com/emacs-evil/evil/issues/301
  (evil-normalize-keymaps))

(evil-ts-obj--bind-keys)


(provide 'evil-ts-obj-evil)
;;; evil-ts-obj-evil.el ends here
