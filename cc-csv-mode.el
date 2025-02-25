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

(add-hook 'csv-mode-hook
          (lambda ()
            (visual-line-mode -1)))


(transient-define-prefix casual-csv-tmenu ()
  ["Casual CSV"

   ["Display"
    ("a" "Align" csv-align-fields)
    ("u" "Unalign" csv-unalign-fields)]
   ]
  )

(keymap-set csv-mode-map "M-m" #'casual-csv-tmenu)


(provide 'cc-csv-mode)
;;; cc-csv-mode.el ends here
