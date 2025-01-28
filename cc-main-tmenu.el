;;; cc-main-tmenu.el --- Main Menu                    -*- lexical-binding: t; -*-

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
(require 'casual-editkit)
(require 'cclisp)
(require 'password-store-menu)
(require 'google-translate-smooth-ui)
(require 'webpaste)
(require 'markdown-mode)

(defvar cc-main-tmenu-customize-enable t
  "If t then enable Casual menu customizations.")

(when (and cc-main-tmenu-customize-enable (not (featurep 'cc-main-tmenu)))
  ;; modify `casual-editkit-main-tmenu'
  (transient-append-suffix 'casual-editkit-main-tmenu "R"
    '("j" "Goto Journal…" cc/select-journal-file))

  (transient-append-suffix 'casual-editkit-main-tmenu "C-o"
    '("I" "Korean Input"
      (lambda () (interactive)(set-input-method 'korean-hangul))
      :transient nil))

  (transient-append-suffix 'casual-editkit-main-tmenu "I"
    '("1" "Translate"
      google-translate-smooth-translate
      :inapt-if-not use-region-p
      :transient nil))

  ;; (transient-append-suffix 'casual-editkit-main-tmenu "C"
  ;;   '("!" "Shell Command…" shell-command))

  ;; modify `casual-editkit-tools-tmenu'
  (transient-append-suffix 'casual-editkit-tools-tmenu "w"
    '("P" "Password›" password-store-menu))

  (transient-append-suffix 'casual-editkit-tools-tmenu "M-e"
    '("C-p" "Call" cc/call-nanp-phone-number
      :inapt-if-not use-region-p))

  (transient-append-suffix 'casual-editkit-tools-tmenu "C-p"
    '("m" "Maps" cc/open-region-in-apple-maps
      :inapt-if-not use-region-p))

  (transient-append-suffix 'casual-editkit-tools-tmenu "m"
    '("M-s" "Say" cc/say-region
      :inapt-if-not use-region-p))

  (transient-append-suffix 'casual-editkit-tools-tmenu "M-s"
    '("M-t" "Translate" google-translate-smooth-translate
      :inapt-if-not use-region-p))

  (transient-append-suffix 'casual-editkit-tools-tmenu "M-t"
    '("M-p" "Webpaste" webpaste-paste-region
      :inapt-if-not use-region-p))

  (transient-append-suffix 'casual-editkit-tools-tmenu "z"
    '("F" "Fireplace" fireplace))

  (transient-append-suffix 'casual-editkit-tools-tmenu "F"
    '("Z" "Snow" snow))

  (transient-append-suffix 'casual-editkit-narrow-tmenu '(0 0)
   ["Markdown"
    :if (lambda () (derived-mode-p 'markdown-mode))
    ("s" "Subtree" markdown-narrow-to-subtree)
    ("b" "Block" markdown-narrow-to-block)
    ("p" "Page" markdown-narrow-to-page)]))

(provide 'cc-main-tmenu)
;;; cc-main-tmenu.el ends here
