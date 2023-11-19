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

(defun evil-ts-obj-common-param-around-mod (node)
  (when-let* ((end-pos (treesit-node-end node))
              (sibling-or-sep (treesit-node-next-sibling node)))
    (if (not (equal (treesit-node-type sibling-or-sep) ","))
        ;; maybe lang without separators
        (setq end-pos (treesit-node-start sibling-or-sep))

      (setq end-pos (treesit-node-end sibling-or-sep))
      ;; try find next sibling param
      (setq sibling-or-sep (treesit-node-next-sibling sibling-or-sep))
      (when sibling-or-sep
        (setq end-pos (treesit-node-start sibling-or-sep))))

    (list (treesit-node-start node)
          end-pos)))


(provide 'evil-ts-obj-common)
;;; evil-ts-obj-common.el ends here
