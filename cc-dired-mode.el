;;; cc-dired-mode.el --- Dired Customization -*- lexical-binding: t -*-

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
(require 'dired)
(require 'dired-x)
(require 'dired-async)
(require 'cclisp)
(require 'wdired)
(require 'image-dired)
(require 'image-dired-dired)
(require 'casual-dired)
(require 'casual-editkit)
(require 'dired-rsync-transient)
(require 'avy)

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'context-menu-mode)
(add-hook 'dired-mode-hook #'dired-async-mode)
(add-hook
 'dired-mode-hook
 (lambda ()
   (setq-local mouse-1-click-follows-link 'double)))

(keymap-set dired-mode-map "M-o" #'dired-omit-mode)
(keymap-set dired-mode-map "C-M-o" #'casual-editkit-main-tmenu)
(keymap-set dired-mode-map "E" #'wdired-change-to-wdired-mode)
(keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
(keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu)
(keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu)
(keymap-set dired-mode-map "M-n" #'dired-next-dirline)
(keymap-set dired-mode-map "M-p" #'dired-prev-dirline)
(keymap-set dired-mode-map "]" #'dired-next-subdir)
(keymap-set dired-mode-map "[" #'dired-prev-subdir)
(keymap-set dired-mode-map "M-]" #'dired-next-marked-file)
(keymap-set dired-mode-map "M-[" #'dired-prev-marked-file)
(keymap-set dired-mode-map "M-j" #'dired-goto-subdir)
(keymap-set dired-mode-map ";" #'image-dired-dired-toggle-marked-thumbs)
(keymap-set dired-mode-map "<f1>" #'avy-goto-end-of-line)
(keymap-set dired-mode-map "." #'dired-up-directory)
(keymap-set dired-mode-map "M-m" #'dired-rsync-transient)
(keymap-set dired-mode-map "M-l" #'dired-other-window)
(keymap-set dired-mode-map "C-c e" #'casual-dired-elisp-tmenu)

;; Added to be consistent with IBuffer
(keymap-set dired-mode-map "<backtab>" #'dired-prev-subdir)
(keymap-set dired-mode-map "TAB" #'dired-next-subdir)

(defun cc/dired-mouse-toggle-mark ()
  "Toggle mark of a Dired item via mouse."
  (interactive)
  (unless (use-region-p)
    (mouse-set-point last-input-event)
    (if (char-equal (char-after (line-beginning-position)) dired-marker-char)
        (call-interactively #'dired-unmark)
      (call-interactively #'dired-mark))))

(keymap-set dired-mode-map "A-M-<mouse-1>" #'browse-url-of-dired-file)
(keymap-set dired-mode-map "M-<mouse-1>" #'cc/dired-mouse-toggle-mark)

(keymap-set image-dired-thumbnail-mode-map "n" #'image-dired-display-next)
(keymap-set image-dired-thumbnail-mode-map "p" #'image-dired-display-previous)

(add-hook 'wdired-mode-hook #'superword-mode)

(defun cc/casual-dired-subsystem (subsystem)
  "Create Dired buffer in Casual project filtering SUBSYSTEM."
  (interactive "sSub-system: ")
  (let* ((pattern (format ".%s.*el$" subsystem))
         (bname (format "*casual-%s*" subsystem)))
    (if (get-buffer bname)
        (kill-buffer bname))
    (find-file "~/Projects/elisp/casual")
    (casual-dired-find-dired-regexp pattern)
    (rename-buffer bname)))

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
