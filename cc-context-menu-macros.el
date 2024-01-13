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

(defun cc/context-menu-label (prefix)
  "Generate context menu label with region string prepended by PREFIX."
  (let* ((start (region-beginning))
        (end (region-end))
        (buf "")
        (max 25)
        (size (abs (- start end))))
    (if (> size max)
        (setq buf (concat prefix " “"(buffer-substring start (+ max start)) "…”"))
      (setq buf (concat prefix " “" (buffer-substring start end) "”")))
    buf))

(defun cc/context-menu-last-word-in-region (prefix)
  "Generate context menu label with last word in region prepended by PREFIX."
  (let*  ((start (region-beginning))
         (end (region-end))
         (buf (buffer-substring start end))
         (last-word (car (last (split-string buf " ")))))
    (concat prefix " “" last-word "”")))

(defun cc/org-stored-links-p ()
  "Predicate if `org-stored-links' is populated.
Return t if populated, nil otherwise."
  (if (> (length org-stored-links) 0)
      t
    nil))

(provide 'cc-context-menu-macros)
;;; cc-context-menu-macros.el ends here
