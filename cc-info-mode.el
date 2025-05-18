;;; cc-info-mode.el --- Info mode customization      -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025  Charles Choi

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
(require 'info)
(require 'casual-info)
(require 'casual-editkit)

(defun cc/clone-in-new-tab ()
  (interactive)
  (unless (fboundp #'tab-bar-mode) (error "Error: tab-bar-mode not available."))
  (unless (derived-mode-p 'Info-mode) (error "Error: must only be run in Info."))
  (tab-new-to -1)
  (clone-buffer nil t)
  (delete-other-windows))

(defun cc/rename-to-info-manual ()
  "Rename buffer to *info <manual name>*."
  (interactive)
  (unless (derived-mode-p 'Info-mode) (error "Error: can only run in Info buffer."))
  (let ((manual-name (file-name-nondirectory Info-current-file)))
    (rename-buffer (format "*info %s*" manual-name))))

(keymap-set Info-mode-map "C-o" #'casual-info-tmenu)
(keymap-set Info-mode-map "C-M-o" #'casual-editkit-main-tmenu)

(keymap-set Info-mode-map "<mouse-5>" #'Info-history-forward)
(keymap-set Info-mode-map "<mouse-4>" #'Info-history-back)
(keymap-set Info-mode-map "s-<tab>" #'cc/clone-in-new-tab)
(keymap-set Info-mode-map "M-t" #'cc/clone-in-new-tab)

(provide 'cc-info-mode)
;;; cc-info-mode.el ends here
