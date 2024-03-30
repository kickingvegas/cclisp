;;; cc-doc-mode-ux.el --- Doc Mode UX Mods -*- lexical-binding: t; -*-

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
;;(define-key Info-mode-map (kbd "q") 'cc/confirm-before-quit-window)
;; Use web-browser history navigation bindings
(define-key Info-mode-map (kbd "M-[") 'Info-history-back)
(define-key Info-mode-map (kbd "M-]") 'Info-history-forward)
;; Bind p and n to paragraph navigation
(define-key Info-mode-map (kbd "p") 'cc/docview-backward-paragraph)
(define-key Info-mode-map (kbd "n") 'cc/docview-forward-paragraph)
;; Bind <f1> to help
(define-key Info-mode-map (kbd "<f1>") 'Info-help)
;; Bind M-j, M-k to scrolling up/down line
(define-key Info-mode-map (kbd "M-j") 'scroll-up-line)
(define-key Info-mode-map (kbd "M-k") 'scroll-down-line)
;; Bind h and l to navigate to previous and next nodes
;; Bind j and k to navigate to next and previous references
(define-key Info-mode-map (kbd "h") 'Info-prev)
(define-key Info-mode-map (kbd "j") 'Info-next-reference)
(define-key Info-mode-map (kbd "k") 'Info-prev-reference)
(define-key Info-mode-map (kbd "l") 'Info-next)
;; Bind / to search
(define-key Info-mode-map (kbd "/") 'Info-search)
;; Set Bookmark
(define-key Info-mode-map (kbd "B") 'bookmark-set)
;; Bind side mouse buttons on Logitech mouse
(define-key Info-mode-map (kbd "<mouse-5>") 'Info-history-forward)
(define-key Info-mode-map (kbd "<mouse-4>") 'Info-history-back)

(add-hook 'Info-mode-hook 'hl-line-mode)
(add-hook 'Info-mode-hook 'visual-line-mode)
(add-hook 'Info-mode-hook 'scroll-lock-mode)

;; # Help
;; Use web-browser history navigation bindings
(define-key help-mode-map (kbd "M-[") 'help-go-back)
(define-key help-mode-map (kbd "M-]") 'help-go-forward)
;; Bind p and n to paragraph navigation
(define-key help-mode-map (kbd "p") 'cc/docview-backward-paragraph)
(define-key help-mode-map (kbd "n") 'cc/docview-forward-paragraph)
;; Bind <f1> to help
(define-key help-mode-map (kbd "<f1>") 'describe-mode)
;; Bind M-j, M-k to scrolling up/down line
(define-key help-mode-map (kbd "M-j") 'scroll-up-line)
(define-key help-mode-map (kbd "M-k") 'scroll-down-line)
;; Bind j and k to navigate to forward and backward buttons
(define-key help-mode-map (kbd "j") 'forward-button)
(define-key help-mode-map (kbd "k") 'backward-button)
;; Bind side mouse buttons on Logitech mouse
(define-key help-mode-map (kbd "<mouse-5>") 'help-go-forward)
(define-key help-mode-map (kbd "<mouse-4>") 'help-go-back)

(add-hook 'help-mode-hook 'hl-line-mode)
(add-hook 'help-mode-hook 'visual-line-mode)
(add-hook 'help-mode-hook 'scroll-lock-mode)

;; # Shortdoc
;; Bind <f1> to help
(define-key shortdoc-mode-map (kbd "<f1>") 'describe-mode)
;; Bind M-j, M-k to scrolling up/down line
(define-key shortdoc-mode-map (kbd "M-j") 'scroll-up-line)
(define-key shortdoc-mode-map (kbd "M-k") 'scroll-down-line)
;; Bind h and l to navigate to previous and next sections
;; Bind j and k to navigate to next and previous
(define-key shortdoc-mode-map (kbd "h") 'shortdoc-previous-section)
(define-key shortdoc-mode-map (kbd "j") 'shortdoc-next)
(define-key shortdoc-mode-map (kbd "k") 'shortdoc-previous)
(define-key shortdoc-mode-map (kbd "l") 'shortdoc-next-section)

(add-hook 'shortdoc-mode-hook 'hl-line-mode)
(add-hook 'shortdoc-mode-hook 'visual-line-mode)
(add-hook 'shortdoc-mode-hook 'scroll-lock-mode)

;; # Man
;; Bind <f1> to help
(define-key Man-mode-map (kbd "<f1>") 'describe-mode)
;; Bind M-j, M-k to scrolling up/down line
(define-key Man-mode-map (kbd "M-j") 'scroll-up-line)
(define-key Man-mode-map (kbd "M-k") 'scroll-down-line)
;; Bind j and k to navigate forward and backward paragraphs
(define-key Man-mode-map (kbd "j") 'cc/docview-forward-paragraph)
(define-key Man-mode-map (kbd "k") 'cc/docview-backward-paragraph)
;; Bind K to kill buffer to replace override of default k above
(define-key Man-mode-map (kbd "K") 'Man-kill)

(add-hook 'Man-mode-hook 'hl-line-mode)
(add-hook 'Man-mode-hook 'visual-line-mode)
(add-hook 'Man-mode-hook 'scroll-lock-mode)

(provide 'cc-doc-mode-ux)

;;; cc-doc-mode-ux.el ends here
