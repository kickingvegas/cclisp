;;; cc-find-menu.el --- find menu -*- lexical-binding: t; -*-

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

;;; Code:

(easy-menu-define cc/find-menu nil
  "Keymap for Find submenu."
  '("Find and/or Replace"
    ["Find in files (rgrep)…" rgrep
     :help "Recursively find for Emacs regex in files rooted at \
current buffer"]

    ["Query Replace…" query-replace
     :help "Replace some occurrences of from-string with \
to-string"]

    ["Query Replace Regex…" query-replace-regexp
     :help "Replace some occurrences of Emacs regex with \
to-string"]

    ["Regex Builder…" re-builder
     :help "Construct an Emacs regex interactively"]))

(provide 'cc-find-menu)

;;; cc-find-menu.el ends here
