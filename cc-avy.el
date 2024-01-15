;;; cc-avy.el --- Avy Customization                 -*- lexical-binding: t; -*-

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
(require 'transient)
(require 'avy)
(require 'display-line-numbers)
(require 'org)

(defun cc/display-line-numbers-mode-p ()
  "Predicate to test if `display-line-numbers-mode' is enabled."
  (symbol-value display-line-numbers))

(defun cc/org-mode-p ()
  "Predicate to test if `org-mode' is enabled."
  (derived-mode-p 'org-mode))

(transient-define-prefix cc/avy-menu ()
  "Avy Transient menu."
  [["Goto Thing"
    ("c"
     "Character"
     avy-goto-char-timer
     :transient nil)
    ("w"
     "Word"
     avy-goto-word-1
     :transient nil)
    ("s"
     "Symbol"
     avy-goto-symbol-1
     :transient nil)]
   ["Goto Line"
    ("l"
     "Line"
     avy-goto-line
     :transient nil)
    ("e"
     "End of Line"
     avy-goto-end-of-line
     :transient nil)
    ("a"
     "Line Above"
     avy-goto-line-above
     :transient nil)
    ("b"
     "Line Below"
     avy-goto-line-below
     :transient nil)
    ("o"
     "Org Heading"
     avy-org-goto-heading-timer
     :if cc/org-mode-p
     :transient nil)
    ("n"
     "Line Number"
     goto-line
     :if cc/display-line-numbers-mode-p
     :transient nil)]
   ["Edit"
    ("C"
     "Copy Other Line"
     avy-kill-ring-save-whole-line
     :transient nil)
    ("k"
     "Kill Other Line"
     avy-kill-whole-line
     :transient nil)
    ("m"
     "Move Other Line to Point"
     avy-move-line
     :transient nil)
    ("d"
     "Duplicate Other Line to Point"
     avy-copy-line
     :transient nil)]])

(provide 'cc-avy)
;;; cc-avy.el ends here
