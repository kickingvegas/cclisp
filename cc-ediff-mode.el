;;; cc-ediff-mode.el --- Ediff configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>

;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(require 'ediff)
(require 'casual-ediff)

(casual-ediff-install)

;; Oh dang, this looks like it works…
;; (defun cc/ediff-show-text-all ()
;;   "Expand all Org headings in the data buffers for Ediff."
;;   (dolist (b (list (and (boundp 'ediff-buffer-A) ediff-buffer-A)
;;                    (and (boundp 'ediff-buffer-B) ediff-buffer-B)
;;                    (and (boundp 'ediff-buffer-C) ediff-buffer-C)))
;;     (when (buffer-live-p b)
;;       (with-current-buffer b
;;         (cond
;;          ((derived-mode-p 'org-mode)
;;           (if (fboundp 'org-fold-show-all) (org-fold-show-all))
;;           (if (fboundp 'visible-mode) (visible-mode nil))
;;           (if (fboundp 'org-remove-inline-images) (org-remove-inline-images)))

;;          ((derived-mode-p 'markdown-mode)
;;           (if (fboundp 'outline-show-all) (outline-show-all))
;;           (if (fboundp 'markdown-toggle-markup-hiding)
;;               (markdown-toggle-markup-hiding -1))
;;           (if (fboundp 'markdown-remove-inline-images)
;;               (markdown-remove-inline-images)))

;;          (t nil))))))

;; (add-hook 'ediff-startup-hook #'cc/ediff-show-text-all)

(defun cc/ediff-text-mode-hook ()
  "Hook for revealing text mode."
  (cond
   ((derived-mode-p 'org-mode)
    (if (fboundp 'org-fold-show-all) (org-fold-show-all))
    (if (fboundp 'visible-mode) (visible-mode nil))
    (if (fboundp 'org-remove-inline-images) (org-remove-inline-images)))

   ((derived-mode-p 'markdown-mode)
    (if (fboundp 'outline-show-all) (outline-show-all))
    (if (fboundp 'markdown-toggle-markup-hiding)
        (markdown-toggle-markup-hiding -1))
    (if (fboundp 'markdown-remove-inline-images)
        (markdown-remove-inline-images)))

   (t (if (fboundp 'outline-show-all) (outline-show-all)))))

(add-hook 'ediff-prepare-buffer-hook #'cc/ediff-text-mode-hook)

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu)))

;; (add-hook 'ediff-after-setup-windows-hook (lambda () (call-interactively
;;                                                  'casual-ediff-tmenu)))

;;(add-hook 'ediff-mode-hook (lambda () (call-interactively casual-ediff-tmenu)))

(provide 'cc-ediff-mode)
;;; cc-ediff-mode.el ends here
