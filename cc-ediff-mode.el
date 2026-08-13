;;; cc-ediff-mode.el --- Ediff configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Charles Choi

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

(defvar cc/--ediff-refresh nil
  "Refresh variable to test if update is required.")

(defun cc/ediff-before-setup ()
  "Hook function to run before Ediff rearranges windows."
  (when (buffer-narrowed-p)
    (setq cc/--ediff-refresh t)
    (widen)))

(defun cc/ediff-prepare-buffer ()
  "Hook for preparing buffer."
  ;; TODO: figure out how to checkpoint and restore visual presentation.
  (if (and (bound-and-true-p hs-minor-mode)
           (fboundp 'hs-show-all))
      (hs-show-all))

  (cond
   ((derived-mode-p 'org-mode)
    (if (fboundp 'org-fold-show-all)
        (org-fold-show-all))
    (if (fboundp 'visible-mode)
        (visible-mode nil))
    (if (fboundp 'org-remove-inline-images)
        (org-remove-inline-images)))

   ((derived-mode-p 'markdown-mode)
    (if (fboundp 'outline-show-all)
        (outline-show-all))
    (if (fboundp 'markdown-toggle-markup-hiding)
        (markdown-toggle-markup-hiding -1))
    (if (fboundp 'markdown-remove-inline-images)
        (markdown-remove-inline-images)))

   (t (if (fboundp 'outline-show-all)
          (outline-show-all)))))

(defun cc/ediff-startup ()
  "Hook run at end of Ediff startup."
  ;; This is a hack to recompute the diff block due to narrowing.
  (if cc/--ediff-refresh
      (ediff-update-diffs)))

(add-hook 'ediff-before-setup-hook #'cc/ediff-before-setup)
(add-hook 'ediff-prepare-buffer-hook #'cc/ediff-prepare-buffer)
(add-hook 'ediff-startup-hook #'cc/ediff-startup)


(provide 'cc-ediff-mode)
;;; cc-ediff-mode.el ends here
