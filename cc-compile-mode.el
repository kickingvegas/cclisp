;;; cc-compile-mode.el --- grep mode customization      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

(add-hook 'compilation-mode-hook #'hl-line-mode)
(add-hook 'compilation-mode-hook #'goto-address-mode)

(keymap-set compilation-mode-map "C-o" #'casual-compile-tmenu)
(keymap-set compilation-mode-map "M-m" #'casual-compile-tmenu)
(keymap-set compilation-mode-map "k" #'compilation-previous-error)
(keymap-set compilation-mode-map "j" #'compilation-next-error)
(keymap-set compilation-mode-map "o" #'compilation-display-error)
(keymap-set compilation-mode-map "[" #'compilation-previous-file)
(keymap-set compilation-mode-map "]" #'compilation-next-file)

(provide 'cc-compile-mode)
;;; cc-compile-mode.el ends here
