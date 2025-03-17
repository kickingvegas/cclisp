;;; cc-make-mode.el --- makefile-mode configuration   -*- lexical-binding: t; -*-

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
(require 'make-mode)
(require 'compile)
(require 'casual-make)

(keymap-set makefile-mode-map "<f9>" #'compile)
(keymap-set makefile-mode-map "C-6" #'imenu)

;;(add-hook 'makefile-mode-hook #'makefile-gmake-mode)

(add-hook 'makefile-mode-hook #'imenu-add-menubar-index)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local imenu-auto-rescan t)
            (setq-local imenu-sort-function #'imenu--sort-by-name)))

(keymap-set makefile-mode-map "M-m" #'casual-make-tmenu)
(keymap-set makefile-mode-map "C-c m" #'casual-make-tmenu)

(provide 'cc-make-mode)
;;; cc-make-mode.el ends here
