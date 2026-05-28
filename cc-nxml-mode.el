;;; cc-nxml-mode.el --- nXML Mode Customization      -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Choi

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
(require 'nxml-mode)
(require 'hl-line)
(require 'casual-lib)

(keymap-set nxml-mode-map "M-[" #'backward-sexp)
(keymap-set nxml-mode-map "M-]" #'forward-sexp)
(keymap-set nxml-mode-map "C-<down>" #'nxml-down-element)
(keymap-set nxml-mode-map "C-<up>" #'nxml-backward-up-element)
(keymap-set nxml-mode-map "C-<left>" #'nxml-backward-element)
(keymap-set nxml-mode-map "C-<right>" #'nxml-forward-element)

(add-hook 'nxml-mode-hook #'hl-line-mode)

(transient-define-prefix casual-nxml-tmenu ()
  "Transient menu for nxml."
  :refresh-suffixes t
  ["Casual: nxml"


   ["</>"
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("i" "Insert…" sgml-tag)
    ;; ("a" "Attribute(s)…" sgml-attributes)
    ("c" "Close" nxml-finish-element)
    ("d" "Delete" sgml-delete-tag
     :if-not (lambda () (derived-mode-p 'html-ts-mode)))]


   ["nXML"
    ("x" "Insert XML declaration" nxml-insert-xml-declaration)
    ("m" "Dynamic Markup Word" nxml-dynamic-markup-word)
    ]

   ["Hide/Show"
    ("hd" "Hide Direct Text Content" nxml-hide-direct-text-content :transient t)
    ("sd" "Show Direct Text Content" nxml-show-direct-text-content :transient t)

    ("hs" "Hide Subheadings" nxml-hide-subheadings :transient t)
    ("sh" "Show Subheadings" nxml-show-subheadings :transient t)
    ("s2" "Show Direct Subheadings" nxml-show-direct-subheadings :transient t)

    ("ht" "Hide Text Content" nxml-hide-text-content :transient t)
    ("ha" "Hide All Text Content" nxml-hide-all-text-content :transient t)

    ("sa" "Show All" nxml-show-all :transient t)
    ("ss" "Show" nxml-show :transient t)


    ("ho" "Hide Other" nxml-hide-other :transient t)
    ("r" "Refresh Outline" nxml-refresh-outline :transient t)]]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])


(keymap-set nxml-mode-map "M-m" #'casual-nxml-tmenu)



(provide 'cc-nxml-mode)
;;; cc-nxml-mode.el ends here
