;;; cc-text-mode.el --- Text Mode Customization -*- lexical-binding: t; -*-

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

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'context-menu-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'company-mode)

(add-hook 'text-mode-hook (lambda ()
                            (setq-local line-spacing 0.1)))

(keymap-set text-mode-map "A-<left>" #'backward-sentence)
(keymap-set text-mode-map "A-<right>" #'forward-sentence)
(keymap-set text-mode-map "A-M-<left>" #'backward-paragraph)
(keymap-set text-mode-map "A-M-<right>" #'forward-paragraph)
(keymap-set text-mode-map "M-/" #'hippie-expand)

;;(setq auto-mode-alist (cons (cons "\\.txt$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.text$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.mml$" 'text-mode) auto-mode-alist))

(provide 'cc-text-mode)
;;; cc-text-mode.el ends here
