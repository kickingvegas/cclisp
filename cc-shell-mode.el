;;; cc-shell-mode.el --- Shell Mode Customization -*- lexical-binding: t; -*-

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

;;; Code:

(add-hook 'shell-mode-hook 'context-menu-mode)
(keymap-set shell-mode-map "C-p" #'comint-previous-input)
(keymap-set shell-mode-map "C-n" #'comint-next-input)

(keymap-set shell-mode-map "M-b" #'backward-sexp)
(keymap-set shell-mode-map "M-f" #'cc/next-sexp)
(keymap-set shell-mode-map "C-<left>" #'backward-sexp)
(keymap-set shell-mode-map "C-<right>" #'cc/next-sexp)
;; (keymap-set shell-mode-map "C-<up>" #'backward-up-list)
;; (keymap-set shell-mode-map "C-<down>" #'down-list)

(provide 'cc-shell-mode)
;;; cc-shell-mode.el ends here
