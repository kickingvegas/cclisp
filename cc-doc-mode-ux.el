;;; cc-doc-mode-ux.el --- Doc Mode UX Mods -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024, 2026  Charles Choi

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
;; UX modifications for different Emacs documentation modes.
;; Covers Info, Help, Man, and Shortdoc.

;;; Code:
(require 'info)
(require 'help)
(require 'shortdoc)
(require 'man)
(require 'hl-line)
(require 'simple)
(require 'cclisp)

(defun cc/confirm-before-quit-window ()
  "Raise confirm prompt before invoking `quit-window'."
  (interactive)
  (if (y-or-n-p "Really Quit? ")
      (quit-window)
    (message "all good")))

;; # Info
;; Prompt before quitting
;;(keymap-set Info-mode-map "q" 'cc/confirm-before-quit-window)
;; Bind <f1> to help
(keymap-set Info-mode-map "<f1>" #'Info-help)
;; Bind side mouse buttons on Logitech mouse
(keymap-set Info-mode-map "<mouse-5>" #'Info-history-forward)
(keymap-set Info-mode-map "<mouse-4>" #'Info-history-back)

(add-hook 'Info-mode-hook #'hl-line-mode)
(add-hook 'Info-mode-hook #'scroll-lock-mode)

;; # Help
;; Bind <f1> to help
(keymap-set help-mode-map "<f1>" #'describe-mode)
;; Bind M-j, M-k to scrolling up/down line
(keymap-set help-mode-map "M-j" #'scroll-up-line)
(keymap-set help-mode-map "M-k" #'scroll-down-line)
;; Bind side mouse buttons on Logitech mouse
(keymap-set help-mode-map "<mouse-5>" #'help-go-forward)
(keymap-set help-mode-map "<mouse-4>" #'help-go-back)


(add-hook 'help-mode-hook #'hl-line-mode)
(add-hook 'help-mode-hook #'scroll-lock-mode)

;; # Shortdoc
;; Bind <f1> to help
(keymap-set shortdoc-mode-map "<f1>" #'describe-mode)
;; Bind M-j, M-k to scrolling up/down line
(keymap-set shortdoc-mode-map "M-j" #'scroll-up-line)
(keymap-set shortdoc-mode-map "M-k" #'scroll-down-line)
;; Bind h and l to navigate to previous and next sections
;; Bind j and k to navigate to next and previous
(keymap-set shortdoc-mode-map "h" #'shortdoc-previous-section)
(keymap-set shortdoc-mode-map "j" #'shortdoc-next)
(keymap-set shortdoc-mode-map "k" #'shortdoc-previous)
(keymap-set shortdoc-mode-map "l" #'shortdoc-next-section)

(add-hook 'shortdoc-mode-hook #'hl-line-mode)
(add-hook 'shortdoc-mode-hook #'scroll-lock-mode)

;; # Man
;; Bind <f1> to man
(keymap-set Man-mode-map "<f1>" #'man)
;; Bind M-j, M-k to scrolling up/down line
(keymap-set Man-mode-map "M-j" #'scroll-up-line)
(keymap-set Man-mode-map "M-k" #'scroll-down-line)

(add-hook 'Man-mode-hook #'hl-line-mode)
(add-hook 'Man-mode-hook #'scroll-lock-mode)

(provide 'cc-doc-mode-ux)

;;; cc-doc-mode-ux.el ends here
