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
(require 'casual-lib)

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
  "Transient menu for Re-Builder commands.

Menu for Re-Builder (`re-builder'), a tool to construct a
regexp interactively.

* References
- Info node `(elisp) Regular Expressions'"
  :refresh-suffixes t
  ["RE-Builder"
   :description (lambda () (format "RE-Builder (%s)" reb-target-buffer))
   ["Copy Regexp"
    ("w" "Interactive" cc/reb-copy
     :if-not (lambda () (derived-mode-p 'reb-lisp-mode))
     :transient t)
    ("c" "Code" reb-copy :transient t)]

   ["Match"
    ("p" "Previous" reb-prev-match :transient t)
    ("n" "Next" reb-next-match :transient t)]

   ["Change"
    ("x" "Syntax" reb-change-syntax
     :description (lambda () (format "Syntax (%s)" reb-re-syntax))
     :transient t)
    ("b" "Target buffer" reb-change-target-buffer
     :transient t)
    ("t" "Case sensitivity" reb-toggle-case :transient t)]

   ["Display"
    ("s" "Subexp mode" reb-enter-subexp-mode)
    ("f" "Force update" reb-force-update :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("i" "ⓘ Regexp Syntax" cc/reb-info-regexp
           :if (lambda () (derived-mode-p 'reb-mode)))
          ("i" "ⓘ Rx Notation" cc/rx-info-regexp
           :if (lambda () (derived-mode-p 'reb-lisp-mode)))
          ("q" "Quit" reb-quit)
          (casual-lib-quit-all)])

(defun cc/reb-info-regexp ()
  "Get Info for special characters in regular expressions."
  (interactive)
  (info "(elisp) Syntax of Regexps"))

(defun cc/rx-info-regexp ()
  "Get Info for special characters in regular expressions."
  (interactive)
  (info "(elisp) Rx Notation"))

(keymap-set reb-mode-map "C-o" #'cc/reb-tmenu)
(keymap-set reb-lisp-mode-map "C-o" #'cc/reb-tmenu)

(provide 'cc-re-builder)
;;; cc-re-builder.el ends here
