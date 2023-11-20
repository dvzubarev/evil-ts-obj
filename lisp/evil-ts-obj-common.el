;;; evil-ts-obj-common.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Denis Zubarev
;;
;; Author: Denis Zubarev <dvzubarev@yandex.ru>
;; Maintainer: Denis Zubarev <dvzubarev@yandex.ru>
;; Version: 0.0.1
;; Keywords: tools convenience
;; Package-Requires: ((emacs "30.0.50"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provides evil text-objects using tree-sitter
;;
;;; Code:

(require 'evil-ts-obj-conf)

(defun evil-ts-obj-common-param-outer-mod (node)
  (let ((start-pos (treesit-node-start node))
        (end-pos (treesit-node-end node))
        (next-sibling (treesit-node-next-sibling node t))
        (next-sibling-or-sep (treesit-node-next-sibling node))
        sep-found)

    (if next-sibling
        ;; end before next sibling parameter
        (setq end-pos (treesit-node-start next-sibling))
      (when next-sibling-or-sep
        (if (equal (treesit-node-type next-sibling-or-sep) evil-ts-obj-conf-param-sep)
            ;; a separator found
            (setq sep-found t
                  end-pos (treesit-node-end next-sibling-or-sep))
          ;; maybe a closing bracket
          (setq end-pos (treesit-node-start next-sibling-or-sep)))))

    (unless next-sibling
      ;; this is the last parameter
      ;; can we include previous separator?
      (when-let (((not sep-found))
                 (prev-sibling (treesit-node-prev-sibling node t)))

        ;; there was not trailing separator,
        ;; so we can assume that the previous one should be included
        (setq start-pos (treesit-node-end prev-sibling))))
    (list start-pos end-pos)))


(provide 'evil-ts-obj-common)
;;; evil-ts-obj-common.el ends here
