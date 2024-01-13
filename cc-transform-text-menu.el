;;; cc-transform-text-menu.el --- Transform Text Menu -*- lexical-binding: t; -*-

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

(require 'cc-context-menu-macros)

;; (defvar cc/transform-text-menu (make-sparse-keymap "Transform Text")
;;   "Keymap for Transform Text submenu.")

;;; Code:

(easy-menu-define cc/transform-text-menu nil
  "Keymap for Transform Text submenu."
  '("Transform Text"
    :visible (region-active-p)
    ["Make Upper Case" upcase-region
     :enable (region-active-p)
     :help "Convert selected region to upper case"]
    ["Make Lower Case" downcase-region
     :enable (region-active-p)
     :help "Convert selected region to lower case"]
    ["Capitalize" capitalize-region
     :enable (region-active-p)
     :help "Convert the selected region to capitalized form"]))

(provide 'cc-transform-text-menu)
;;; cc-transform-text-menu.el ends here
