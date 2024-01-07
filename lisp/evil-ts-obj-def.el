;;; evil-ts-obj-def.el --- Description -*- lexical-binding: t; -*-
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
;;  Default functions
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'evil-ts-obj-core)


;; * Default functions
;; ** Edit rules

(defun evil-ts-obj-def-conf-lang-raise-rules (range-type &optional text-spec)
  "Return default raise rules for configuration languages like YAML.
See `evil-ts-obj-conf-raise-rules-func' for description of
RANGE-TYPE and TEXT-SPEC."
  (pcase range-type
    ('text
     '((param . inner)
       (compound . outer)))
    ('place
     (pcase text-spec
       ((pmap (:thing 'compound))
        '((param . inner)
          (compound . outer)))
       ((pmap (:thing 'param))
        '((param . all)
          (compound . outer)))))))

(defun evil-ts-obj-def-raise-rules (range-type &optional text-spec)
  "Return default raise rules.
See `evil-ts-obj-conf-raise-rules-func' for description of
RANGE-TYPE and TEXT-SPEC."
  (pcase range-type
    ('text
     '((param . inner)
       (statement . inner)
       (compound . outer)))
    ('place
     (pcase text-spec
       ((pmap (:thing (or 'compound 'statement)))
        '((compound . outer)
          (statement . inner)))
       ((pmap (:thing 'param))
        '((statement . inner)
          (param . all)))))))



;; ** init functions
(defun evil-ts-obj-def-init-lang (lang)
  "Set default values for language LANG."

  (cl-callf plist-put evil-ts-obj-conf-range-finalizers lang
            #'evil-ts-obj--finalize-text-obj-range)

  (cl-callf plist-put evil-ts-obj-conf-raise-rules-func
    lang #'evil-ts-obj-def-raise-rules)

  (cl-callf plist-put evil-ts-obj-conf-nav-things
    lang '(or param statement compound)))

(defun evil-ts-obj-def-init-conf-lang (lang)
  "Set default values for configuration language LANG (YAML, JSON, etc.)."

  (cl-callf plist-put evil-ts-obj-conf-nav-things
    lang '(or param compound))

  (cl-callf plist-put evil-ts-obj-conf-raise-rules-func
    lang #'evil-ts-obj-def-conf-lang-raise-rules))


(provide 'evil-ts-obj-def)
;;; evil-ts-obj-def.el ends here
