;;; cc-nxml-mode.el --- nXML Mode Customization      -*- lexical-binding: t; -*-

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
(require 'nxml-mode)
(require 'hl-line)

(define-key shell-mode-map (kbd "C-p") 'comint-previous-input)

(keymap-set nxml-mode-map "M-[" #'backward-sexp)
(keymap-set nxml-mode-map "M-]" #'forward-sexp)
(keymap-set nxml-mode-map "M-<down>" #'nxml-down-element)
(keymap-set nxml-mode-map "M-<up>" #'nxml-backward-up-element)
(keymap-set nxml-mode-map "M-<left>" #'backward-sexp)
(keymap-set nxml-mode-map "M-<right>" #'forward-sexp)

(add-hook 'nxml-mode-hook #'hl-line-mode)

(provide 'cc-nxml-mode)
;;; cc-nxml-mode.el ends here
