;;; cc-context-menu-macros.el --- Context Menu Macros -*- lexical-binding: t; -*-

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
(require 'ol)

(defmacro cc/context-menu-item-separator (menu key)
  "Add single line separator to MENU with KEY."
  `(define-key-after ,menu [,key]
     '(menu-item "--single-line")))

(defmacro cc/add-context-menu-item (menu command label help)
  "Add COMMAND to MENU annotated with LABEL and property HELP."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-item-visible (menu command label help visible)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, VISIBLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :visible ,visible)))

(defmacro cc/add-context-menu-item-enable (menu command label help enable)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, ENABLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :enable ,enable)))

(defmacro cc/add-first-context-menu-item (menu command label help)
  "Add first COMMAND to MENU annotated with LABEL and HELP."
  `(define-key ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-submenu (menu submenu label)
  "Add SUBMENU to MENU annotated with LABEL.
SUBMENU is a keymap."
  `(define-key-after ,menu [,submenu]
     (list 'menu-item ,label ,submenu)))

(provide 'cc-context-menu-macros)
;;; cc-context-menu-macros.el ends here
