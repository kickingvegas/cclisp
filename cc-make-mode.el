;;; cc-make-mode.el --- makefile-mode configuration   -*- lexical-binding: t; -*-

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
;; (require 'make-mode)
;; (require 'compile)
(require 'imenu)
(require 'casual-make)

(defun cc-make-setup ()
  "Setup function for Make."

  (setq-local imenu-sort-function #'imenu--sort-by-name)
  (keymap-set makefile-mode-map "<f9>" #'compile)
  (keymap-set makefile-mode-map "C-6" #'imenu)
  (keymap-set makefile-mode-map "C-c m" #'casual-make-tmenu))

(add-hook 'makefile-mode-hook #'cc-make-setup)

(provide 'cc-make-mode)
;;; cc-make-mode.el ends here
