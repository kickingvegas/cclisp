;;; cc-grep-mode.el --- grep mode customization      -*- lexical-binding: t; -*-

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

(require 'grep)
(require 'hl-line)
(require 'cc-main-tmenu)

(add-hook 'grep-mode-hook 'hl-line-mode)

(keymap-set grep-mode-map "C-o" #'casual-editkit-main-tmenu)

(provide 'cc-grep-mode)
;;; cc-grep-mode.el ends here
