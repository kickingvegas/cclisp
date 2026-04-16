;;; cc-compile-mode.el --- grep mode customization      -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Choi

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

(require 'compile)
(require 'hl-line)
(require 'casual-compile)
(require 'goto-addr)
(require 'anju)

(add-hook 'compilation-mode-hook #'hl-line-mode)
(add-hook 'compilation-mode-hook #'goto-address-mode)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(keymap-set compilation-mode-map "C-o" #'casual-compile-tmenu)
(keymap-set compilation-mode-map "M-m" #'casual-compile-tmenu)
(keymap-set compilation-mode-map "k" #'compilation-previous-error)
(keymap-set compilation-mode-map "j" #'compilation-next-error)
(keymap-set compilation-mode-map "o" #'compilation-display-error)
(keymap-set compilation-mode-map "[" #'compilation-previous-file)
(keymap-set compilation-mode-map "]" #'compilation-next-file)

(defun cc/context-menu-compile (menu click)
  "Context menu hook function for compile commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."

  (when (derived-mode-p 'compilation-mode)
    (save-excursion
      (mouse-set-point click)
      (anju-context-menu-item-separator menu compile-separator)

      (easy-menu-add-item menu nil
                          ["Recompile"
                           recompile
                           :label (casual-compile--select-mode-label
                                   "Recompile"
                                   (casual-compile-unicode-get :refresh))
                           :enable (not (casual-compile--compilation-running-p))
                           :help "Recompile"])

      (easy-menu-add-item menu nil
                          ["Compile…"
                           compile
                           :enable
                           (and
                            (not (derived-mode-p 'grep-mode))
                            (not (casual-compile--compilation-running-p)))
                           :help "Recompile"])

      (easy-menu-add-item menu nil
                          ["Kill"
                           kill-compilation
                           :label (casual-compile-unicode-get :kill)
                           :visible (casual-compile--compilation-running-p)])))
  menu)

(provide 'cc-compile-mode)
;;; cc-compile-mode.el ends here
