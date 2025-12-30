;;; cc-html-mode.el --- SGML/HTML Mode Customization  -*- lexical-binding: t; -*-

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

(require 'mhtml-mode)
(require 'casual-html)

(keymap-set html-mode-map "M-m" #'casual-html-tmenu)
(keymap-set html-mode-map "C-c m" #'casual-html-tags-tmenu)

;;(keymap-set html-ts-mode-map "M-m" #'casual-html-tmenu)
;;(keymap-set html-ts-mode-map "C-c m" #'casual-html-tags-tmenu)

;; (keymap-set html-ts-mode-map "C-<up>" #'backward-up-list)
;; ;; (keymap-set html-ts-mode-map "C-<down>" #'down-list)
;; (keymap-set html-ts-mode-map "C-<left>" #'backward-sexp)
;; (keymap-set html-ts-mode-map "C-<right>" #'casual-elisp-next-sexp)


(provide 'cc-html-mode)
;;; cc-html-mode.el ends here
