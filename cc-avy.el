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

(defun cc/buffer-writeable-p ()
  "Predicate to test if buffer is writeable."
  (not buffer-read-only))

(defun cc/buffer-writeable-and-region-active-p ()
  "Predicate to test if buffer is writeable and region is active."
  (and (cc/buffer-writeable-p) (region-active-p)))

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
     :transient nil)
    ("W"
     "Whitespace end"
     avy-goto-whitespace-end
     :transient nil)
    ("p"
     "Pop mark"
     avy-pop-mark
     :transient nil)]
   ["Goto Line"
    ("l"
     "Line"
     avy-goto-line
     :transient nil)
    ("e"
     "End of line"
     avy-goto-end-of-line
     :transient nil)
    ("a"
     "Line above"
     avy-goto-line-above
     :transient nil)
    ("b"
     "Line below"
     avy-goto-line-below
     :transient nil)
    ("o"
     "Org heading"
     avy-org-goto-heading-timer
     :if cc/org-mode-p
     :transient nil)
    ("n"
     "Line number"
     goto-line
     :if cc/display-line-numbers-mode-p
     :transient nil)]
   ["Edit Line"
    ("C"
     "Copy"
     avy-kill-ring-save-whole-line
     :transient nil)
    ("k"
     "Kill"
     avy-kill-whole-line
     :if cc/buffer-writeable-p
     :transient nil)
    ("m"
     "Move to above current line"
     avy-move-line
     :if cc/buffer-writeable-p
     :transient nil)
    ("d"
     "Duplicate to above current line"
     avy-copy-line
     :if cc/buffer-writeable-p
     :transient nil)]]

  ["Edit Region (choose two lines)"
    ("r"
     "Copy"
     avy-kill-ring-save-region
     :transient nil)
    ("K"
     "Kill"
     avy-kill-region
     :if cc/buffer-writeable-p
     :transient nil)
    ("M"
     "Move to above current line"
     avy-move-region
     :if cc/buffer-writeable-p
     :transient nil)
    ("D"
     "Duplicate to above current line"
     avy-copy-region
     :if cc/buffer-writeable-p
     :transient nil)
    ("t"
     "Transpose lines in active region"
     avy-transpose-lines-in-region
     :if cc/buffer-writeable-and-region-active-p
     :transient nil)]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'cc-avy)
;;; cc-avy.el ends here
