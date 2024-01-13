;;; cc-prog-mode.el --- Programming Customizations -*- lexical-binding: t; -*-

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
(require 'prog-mode)
(require 'make-mode)
(require 'mouse)
(require 'elec-pair)
(require 'company)
(require 'cc-save-hooks)
(require 'rainbow-mode)
(require 'display-line-numbers)
(require 'display-fill-column-indicator)
(require 'hl-line)
(require 'gud)


;;; Code:
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'context-menu-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook #'cc/save-hook-delete-trailing-whitespace)

(define-key prog-mode-map [remap indent-for-tab-command]
  #'company-indent-or-complete-common)
(define-key prog-mode-map (kbd "C-a") 'back-to-indentation)

;; GUD - mode preferences
;; (setq gud-mode-hook
;;       '((lambda ()
;; 	  (local-set-key [f7] 'gud-step)
;; 	  (local-set-key [f8] 'gud-next)
;; 	  (local-set-key [f9] 'gud-cont))))

;; # Makefile
(define-key makefile-mode-map (kbd "<f9>") 'compile)

(provide 'cc-prog-mode)
;;; cc-prog-mode.el ends here
