;;; cc-markdown-mode.el --- Markdown customizations -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Charles Choi

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
(require 'markdown-mode)
(require 'face-remap)
(require 'cc-save-hooks)
(require 'org-table)
(defvar markdown-mode-map)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook #'variable-pitch-mode)
;;(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)

(add-hook 'markdown-mode-hook #'turn-on-orgtbl)

(keymap-set markdown-mode-map "C-<up>" #'markdown-backward-same-level)
(keymap-set markdown-mode-map "C-<down>" #'markdown-forward-same-level)
(keymap-set markdown-mode-map "M-v" #'markdown-outline-previous)
(keymap-set markdown-mode-map "C-v" #'markdown-outline-next)
(keymap-set markdown-mode-map "M-<f6>" #'markdown-toggle-inline-images)
;; (define-key markdown-mode-map [f13] 'markdown-preview)

(add-hook 'markdown-mode-hook #'cc/save-hook-delete-trailing-whitespace)

(provide 'cc-markdown-mode)
;;; cc-markdown-mode.el ends here
