;;; cc-csv-mode.el --- CSV Mode Customization        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'csv-mode)
(require 'casual-lib)
(require 'casual-editkit-utils)

(add-hook 'csv-mode-hook
          (lambda ()
            (visual-line-mode -1)))

(add-hook 'csv-mode-hook #'csv-guess-set-separator)
(add-hook 'csv-mode-hook #'csv-align-mode)

(transient-define-prefix casual-csv-tmenu ()
  :refresh-suffixes t
  ["Casual CSV"
   ["Field"
    :pad-keys t
    ("S-TAB" "←" csv-backtab-command :transient t)
    ("TAB" "→" csv-tab-command :transient t)
    ("m" "Mark" mark-sexp)
    ("c" "Copy" casual-editkit-copy-sexp)]

   ["Navigation"
    :pad-keys t
    ("p" "↑" previous-line :transient t)
    ("n" "↓" next-line :transient t)
    ("C-a" "⇤" move-beginning-of-line :transient t)
    ("C-e" "⇥" move-end-of-line :transient t)]

   ["Sort"
    :if (lambda () (not buffer-read-only))
    ("s" "Fields" csv-sort-fields)
    ("N" "Numeric" csv-sort-numeric-fields)
    ("r" "Reverse" csv-reverse-region
     :if use-region-p)]

   ["Fields"
    :if (lambda () (not buffer-read-only))
    ("k" "Kill" csv-kill-fields)
    ("y" "Yank" csv-yank-fields)]

   ["Misc"
    ("a" "Toggle Align" csv-align-mode
     :transient t)
    ("t" "Transpose" csv-transpose
     :if (lambda () (not buffer-read-only)))
    ("S" "Separator…" csv-set-separator)
    ("o" "Occur…" occur)

    ("v" "View" view-mode
     :if (lambda () (not buffer-read-only))
     :transient t)

    ("e" "Edit" View-exit
     :if (lambda () buffer-read-only)
     :transient t)
    ]]
  )

(keymap-set csv-mode-map "M-m" #'casual-csv-tmenu)


(provide 'cc-csv-mode)
;;; cc-csv-mode.el ends here
