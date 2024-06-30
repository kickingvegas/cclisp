;;; cc-symbol-overlay.el --- Configuration for Symbol Overlay  -*- lexical-binding: t; -*-

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
(require 'symbol-overlay)
(require 'casual-lib)

(transient-define-prefix cc/symbol-overlay-tmenu ()
  [["Operations"
    ("h" "Toggle highlight" symbol-overlay-put)
    ("w" "Copy symbol name" symbol-overlay-save-symbol)
    ("r" "Rename symbol name…" symbol-overlay-rename)
    ("q" "Query replace…" symbol-overlay-query-replace)
    ("d" "Jump to definition" symbol-overlay-jump-to-definition)]

   ["Navigation"
    :pad-keys t
    ("p" "Jump previous" symbol-overlay-jump-prev)
    ("n" "Jump next" symbol-overlay-jump-next)
    ("M-p" "Switch previous" symbol-overlay-switch-backward)
    ("M-n" "Switch next" symbol-overlay-switch-forward)
    ("e" "Jump to last mark" symbol-overlay-echo-mark)]

   ["Display"
    ("t" "Toggle in scope" symbol-overlay-toggle-in-scope)]

   ["Remove"
    ("k" "Remove all" symbol-overlay-remove-all)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(keymap-set symbol-overlay-map "C-o" #'cc/symbol-overlay-tmenu)

(provide 'cc-symbol-overlay)
;;; cc-symbol-overlay.el ends here
