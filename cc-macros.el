;;; cc-macros.el --- My Macros -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Choi

;; Author: Charles Choi <charles.choi@yummymelon.com>
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
(defalias 'cc/convert-to-menu-testcase
  (kmacro "C-a C-f c a s u a l t - a d d - t e s t c a s e SPC M-] C-o k SPC # ' C-d M-] SPC t e s t - v e c t o r s C-n C-a"))

(defalias 'cc/casual-suffix-to-test
  (kmacro "C-<down> C-<right> C-<right> C-o c C-<up> C-e <return> ( c a s u a l t - m o c k SPC # ' C-y C-<up> C-p C-<down> : b i n d i n g SPC C-<right> C-o k : c o m m a n d C-<up>"))

(defalias 'cc/casual-unicode-to-test
  (kmacro "C-a C-<down> C-o m ( c a s u a l - o r g - u n i c o d e - g e t SPC C-<up> C-o k C-<up> M-] <return> C-y C-a C-o m ( s t r i n g - e q u a l SPC C-<up> C-o m ( s h o u l d SPC C-<up> C-o e D <return> C-p C-<down> C-o k C-<down> C-o k C-<up> C-<up> C-n C-n C-<down> C-<down> C-<right> C-<right> SPC C-y C-<up> C-<up> C-p C-p C-<down> C-<down> C-o k C-<up> C-<up> C-n C-<down> C-<down> C-<right> C-<right> C-y C-<up> C-<up> C-p C-k C-k TAB"))

(defalias 'cc/keybind-convert
  (kmacro "C-<down> k e y m a p - s e t SPC C-o k s-SPC C-<right> C-o e d p M-d s-SPC C-<up>"))

;; Convert Transient suffix to Org texinfo list item
(defalias 'cc/calc-convert-transient-suffix-to-org-list-item
  (kmacro
   "C-f C-M-f C-M-f SPC : : S-SPC DEL C-M-f C-o e d z ) C-a C-f C-o m ( C-b C-o m { C-b C-o m { C-b C-o m { C-f C-f k b d C-f C-o e d p C-a C-o e d p - SPC C-a C-n"))


;; Inflate Org texinfo list item with Calc info.
(defalias 'cc/calc-inflate-command-with-info
  (kmacro
   "C-e M-x c c / g e t <tab> <return> <return> C-y M-x b a c k w a r d SPC p a r a SPC <return> s-SPC s-SPC M-x u n f i l l SPC p SPC <return> <backspace> <return> C-n"))


(provide 'cc-macros)
;;; cc-macros.el ends here
