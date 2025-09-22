;;; cc-bibtex-mode.el ---  Bibtex Mode    -*- lexical-binding: t; -*-

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
(require 'bibtex)
(require 'hl-line)
(require 'casual-bibtex)

(add-hook 'bibtex-mode-hook 'hl-line-mode)

(keymap-set bibtex-mode-map "C-o" #'casual-bibtex-tmenu)
(keymap-set bibtex-mode-map "<TAB>" #'bibtex-next-field)
(keymap-set bibtex-mode-map "<backtab>" #'previous-line)

(keymap-set bibtex-mode-map "C-n" #'bibtex-next-field)
(keymap-set bibtex-mode-map "M-n" #'bibtex-next-entry)
(keymap-set bibtex-mode-map "M-p" #'bibtex-previous-entry)

(keymap-set bibtex-mode-map "<prior>" #'bibtex-previous-entry)
(keymap-set bibtex-mode-map "<next>" #'bibtex-next-entry)

(keymap-set bibtex-mode-map "C-c C-o" #'bibtex-url)
(keymap-set bibtex-mode-map "C-c C-c" #'casual-bibtex-fill-and-clean)

(keymap-set bibtex-mode-map "<clear>" #'bibtex-empty-field)
(keymap-set bibtex-mode-map "M-<clear>" #'bibtex-kill-field)
(keymap-set bibtex-mode-map "M-DEL" #'bibtex-kill-field)

(provide 'cc-bibtex-mode)
;;; cc-bibtex-mode.el ends here
