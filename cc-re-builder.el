;;; cc-re-builder.el --- Re-Builder Customization   -*- lexical-binding: t; -*-

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
(require 're-builder)
(require 'rx)
(require 'transient)

(defun cc/reb-copy ()
  "Reformat `reb-copy' result for interactive use.

The implementation of `reb-copy' presumes that its result will be
used in Elisp code and as such escapes certain characters.

Often it is desired to instead use the regexp in an interactive
function such as `query-replace-regexp'. Such functions require
that the regexp not be escaped, which motivates the need for this
function."
  (interactive)
  (reb-copy)
  (let* ((buf (pop kill-ring))
         (buf (string-trim-left buf "\""))
         (buf (string-trim-right buf "\""))
         (buf (replace-regexp-in-string (rx "\\" (group anything)) "\\1" buf)))
    (kill-new buf)))

(transient-define-prefix cc/reb-tmenu ()
  ["Re-Builder"
   ["Copy Regexp"
    ("c" "For interactive" cc/reb-copy)
    ("C" "For code" reb-copy)]

   ["Match"
    ("p" "Previous" reb-prev-match :transient t)
    ("n" "Next" reb-next-match :transient t)]

   ["Settings"
    ("i" "Change syntax" reb-change-syntax)
    ("b" "Change target buffer" reb-change-target-buffer)
    ("t" "Toggle case" reb-toggle-case)]]

  [:class transient-row
          ("Q" "Quit Re-Builder" reb-quit)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(keymap-set reb-mode-map "C-o" #'cc/reb-tmenu)

(provide 'cc-re-builder)
;;; cc-re-builder.el ends here
