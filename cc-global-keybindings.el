;;; cc-global-keybindings.el --- Global Keybindings -*- lexical-binding: t; -*-

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
;; Global Keyboard Mappings for Charles Choi

;;; Code:

(require 'cc-ediff-mode)
(require 'helm-buffers)
(require 'neotree)
(require 'bookmark)
(require 'avy)
(require 'expand-region)
(require 'cclisp)
(require 'osx-dictionary)
(require 'google-this)
(require 'treemacs)
(require 'neotree)
(require 'python)
(require 'ibuffer)
(require 'magit-status)
(require 'casual-avy)
(require 'cc-main-tmenu)
(require 'casual-editkit)
(require 'minibuffer)

(keymap-global-set "C-=" #'er/expand-region)
;(keymap-global-set (kbd "M-g") 'goto-line)
(keymap-global-set "C-x v" #'view-file)
(keymap-global-set "M-q" #'query-replace)
(keymap-global-set "M-j" #'cc/journal-entry)

;;(keymap-global-set "\C-h" 'delete-backward-char)
;;(keymap-global-set "\C-xh" 'help-for-help)
;;(keymap-global-set "\r" 'newline-and-indent)

(keymap-global-set "<f1>" #'save-buffer)
(keymap-global-set "<f2>" #'ibuffer)
(keymap-global-set "s-<return>" #'other-window)
(keymap-global-set "<f3>" #'save-buffers-kill-emacs)
(keymap-global-set "M-<f3>" #'casual-editkit-windows-tmenu)
(keymap-global-set "<f4>" #'bookmark-jump)
(keymap-global-set "S-<f5>" #'cc/select-journal-file)
(keymap-global-set "<f5>" #'journal)
(keymap-global-set "s-<f5>" #'cc/org-search)
(keymap-global-set "<f6>" #'osx-dictionary-search-input)
(keymap-global-set "<f7>" #'repeat)
(keymap-global-set "M-<f7>" #'repeat-complex-command)
(keymap-global-set "C-o" #'casual-editkit-main-tmenu)
(if (string-equal (window-system) "mac")
    (keymap-global-set "<f10>" #'casual-editkit-main-tmenu))

(keymap-global-set "<f8>" #'org-capture)
(keymap-global-set "<f9>" #'compile)
(keymap-global-set "<f11>" #'bookmark-set-no-overwrite)

(keymap-global-set "M-<f1>" #'cc/open-url)
(keymap-global-set "M-<f2>" #'google-this)
(keymap-global-set "C-c C-;" #'shell-command)
(keymap-global-set "M-<f4>" #'calc)

(keymap-global-set "<f13>" #'google-this)
(keymap-global-set "M-<f13>" #'neotree-toggle)
(keymap-global-set "C-<f13>" #'treemacs)
(keymap-global-set "<f14>" #'eshell) ;regular
;;(keymap-global-set (kbd "<f14>") 'save-buffer) ;logitech
(keymap-global-set "<f15>" #'cc/ediff-revision)

(keymap-global-set "<f16>" #'calc)
(keymap-global-set "<f17>" #'run-python)
(keymap-global-set "M-<f16>" #'cc/switch-to-scratch) ;; this need to be in main
(keymap-global-set "C-x C-b" #'ibuffer)

;; Avy
(keymap-global-set "M-g" #'casual-avy-tmenu)

;; Terminal
(keymap-global-set "M-SPC" #'set-mark-command)
(keymap-global-set "M-c" #'kill-ring-save)

;;(keymap-global-set (kbd "<f8>") 'next-error)
;;(keymap-global-set (kbd "M-<f8>") 'previous-error)
;;(keymap-global-set (kbd "<f9>") 'compile)

(keymap-global-set "C-<f2>" #'delete-other-windows)
(keymap-global-set "C-<f3>" #'kill-buffer)
(keymap-global-set "C-<f4>" #'view-file)
(keymap-global-set "C-z" #'pop-global-mark)

(keymap-global-set "<home>" #'beginning-of-buffer)
(keymap-global-set "<end>" #'end-of-buffer)
(keymap-global-set "C-<home>" #'beginning-of-line)
(keymap-global-set "C-<end>" #'end-of-line)
(keymap-global-set "S-<home>" #'back-to-indentation)
(keymap-global-set "S-<end>" #'end-of-line)
(keymap-global-set "<PageDown>" #'scroll-down-command)
(keymap-global-set "<PageUp>" #'scroll-up-command)
(keymap-global-set "M-z" #'undo)
(keymap-global-set "<delete>" #'delete-forward-char)

;; (when (string-equal (window-system) "mac")
;;   (keymap-global-set "C-<tab>" #'mac-next-tab)
;;   ;; this binding breaks terminal behavior
;;   ;(keymap-global-set (kbd "M-]") 'mac-next-tab)
;;   ;(keymap-global-set (kbd "M-[") 'mac-previous-tab)
;;   (define-key-after global-map
;;     [menu-bar file mac-toggle]
;;     '("Toggle Tab Bar" . mac-toggle-tab-bar) 'close-tab))

;; Left Side Keys
;; (keymap-global-set (kbd "<f11>") 'shell)
;; (keymap-global-set (kbd "C-<f11>")  'shell-new)

;; Keypad Keys
(keymap-global-set "M-<right>" #'forward-word)
(keymap-global-set "M-<left>" #'backward-word)
(keymap-global-set "s-<right>" #'forward-word)
(keymap-global-set "s-<left>" #'backward-word)
(keymap-global-set "s-<up>" #'scroll-one-line-down)
(keymap-global-set "s-<down>" #'scroll-one-line-up)
(keymap-global-set "C-<kp-add>" #'enlarge-window)
(keymap-global-set "C-<kp-subtract>" #'shrink-window)
(keymap-global-set "M-<kp-add>" #'enlarge-window-horizontally)
(keymap-global-set "M-<kp-subtract>" #'shrink-window-horizontally)
(keymap-global-set "C-<kp-enter>" #'other-window)
(keymap-global-set "M-<kp-enter>" #'switch-to-buffer)
(keymap-global-set "C-<return>" #'other-window)

;; Mouse Button Bindings
(keymap-global-set "<mouse-5>" #'next-buffer)
(keymap-global-set "<mouse-4>" #'previous-buffer)

(keymap-global-set "<mode-line> <mouse-2>" #'transpose-frame)

;; Magit
(keymap-global-set "C-x g" #'magit-status)

(keymap-global-unset "C-x f")
;;(global-unset-key (kbd "C-x f"))

;; Find in files (rgrep)
(keymap-global-set "M-F" #'rgrep)

(global-set-key [vertical-scroll-bar down-mouse-1] #'scroll-bar-drag)

;; Window Navigation
(keymap-global-set "C-<kp-8>" #'windmove-up)
(keymap-global-set "C-<kp-5>" #'windmove-down)
(keymap-global-set "C-<kp-2>" #'windmove-down)
(keymap-global-set "C-<kp-4>" #'windmove-left)
(keymap-global-set "C-<kp-6>" #'windmove-right)
(keymap-global-set "C-<kp-0>" #'ace-select-window)
(keymap-global-set "C-<kp-divide>" #'transpose-frame)

(keymap-set minibuffer-local-shell-command-map "M-b" #'backward-sexp)
(keymap-set minibuffer-local-shell-command-map "M-f" #'cc/next-sexp)
(keymap-set minibuffer-local-shell-command-map "C-M-b" #'backward-word)
(keymap-set minibuffer-local-shell-command-map "C-M-f" #'forward-word)
(keymap-set minibuffer-local-shell-command-map "C-<left>" #'backward-sexp)
(keymap-set minibuffer-local-shell-command-map "C-<right>" #'cc/next-sexp)

(keymap-set minibuffer-mode-map "M-b" #'backward-sexp)
(keymap-set minibuffer-mode-map "M-f" #'cc/next-sexp)
(keymap-set minibuffer-mode-map "C-M-b" #'backward-word)
(keymap-set minibuffer-mode-map "C-M-f" #'forward-word)

(keymap-set minibuffer-mode-map "C-<left>" #'backward-sexp)
(keymap-set minibuffer-mode-map "C-<right>" #'cc/next-sexp)
(keymap-set minibuffer-mode-map "C-<up>" #'backward-up-list)
(keymap-set minibuffer-mode-map "C-<down>" #'down-list)

(keymap-global-set "M-\\" #'cycle-spacing)

(provide 'cc-global-keybindings)
;;; cc-global-keybindings.el ends here
