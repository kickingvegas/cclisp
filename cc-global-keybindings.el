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

(global-set-key (kbd "C-=") 'er/expand-region)
;(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x v") 'view-file)
(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "M-j") 'cc/journal-entry)
;(global-set-key "\C-h" 'delete-backward-char) (global-set-key "\C-xh"
;'help-for-help) (global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "A-<return>") 'other-window)
(global-set-key (kbd "<f3>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f4>") 'bookmark-jump)
(global-set-key (kbd "<f5>") 'status-report)
(global-set-key (kbd "S-<f5>") 'cc/select-journal-file)
(global-set-key (kbd "A-<f5>") 'cc/org-search)
(global-set-key (kbd "<f6>") 'osx-dictionary-search-input)
(global-set-key (kbd "<f7>") 'cc/meta-search)
(global-set-key (kbd "C-o") 'cc/meta-search)

(global-set-key (kbd "<f8>") 'org-capture)
(global-set-key (kbd "<f9>") 'avy-goto-word-1)
(global-set-key (kbd "<f11>") 'bookmark-set-no-overwrite)

(global-set-key (kbd "M-<f1>") 'cc/open-url)
(global-set-key (kbd "M-<f2>") 'google-this)
(global-set-key (kbd "C-c C-;") 'shell-command)
(global-set-key (kbd "M-<f4>") 'helm-buffers-list)

(global-set-key (kbd "<f13>") 'cc/ediff-revision)
(global-set-key (kbd "M-<f13>") 'neotree-toggle)
(global-set-key (kbd "C-<f13>") 'treemacs)
(global-set-key (kbd "<f14>") 'eshell) ;regular
;;(global-set-key (kbd "<f14>") 'save-buffer) ;logitech
(global-set-key (kbd "<f15>") 'cc/ediff-revision)

(global-set-key (kbd "<f16>") 'run-python)
(global-set-key (kbd "M-<f16>") 'cc/switch-to-scratch)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Avy
(global-set-key (kbd "M-g g") 'avy-goto-word-1)
(global-set-key (kbd "M-g SPC") 'avy-goto-word-1)
(global-set-key (kbd "M-g l") 'avy-goto-line)
(global-set-key (kbd "M-g t") 'avy-goto-char-timer)
(global-set-key (kbd "M-g c") 'avy-copy-line)
(global-set-key (kbd "M-g m") 'avy-move-line)

;; Terminal
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-c") 'kill-ring-save)

;;(global-set-key (kbd "<f8>") 'next-error)
;;(global-set-key (kbd "M-<f8>") 'previous-error)
;;(global-set-key (kbd "<f9>") 'compile)

(global-set-key (kbd "C-<f2>") 'delete-other-windows)
(global-set-key (kbd "C-<f3>") 'kill-buffer)
(global-set-key (kbd "C-<f4>") 'view-file)
(global-set-key (kbd "C-z") 'pop-global-mark)

(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
(global-set-key (kbd "C-<end>") 'end-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)
(global-set-key (kbd "S-<home>") 'back-to-indentation)
(global-set-key (kbd "S-<end>") 'end-of-line)
(global-set-key (kbd "<PageDown>") 'scroll-down-command)
(global-set-key (kbd "<PageUp>") 'scroll-up-command)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "<delete>") 'delete-forward-char)

(when (string-equal (window-system) "mac")
  (global-set-key (kbd "C-<tab>") 'mac-next-tab)
  ;; this binding breaks terminal behavior
  ;(global-set-key (kbd "M-]") 'mac-next-tab)
  ;(global-set-key (kbd "M-[") 'mac-previous-tab)
  (define-key-after global-map
    [menu-bar file mac-toggle]
    '("Toggle Tab Bar" . mac-toggle-tab-bar) 'close-tab))

;; Left Side Keys
;; (global-set-key (kbd "<f11>") 'shell)
;; (global-set-key (kbd "C-<f11>")  'shell-new)

;; Keypad Keys
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "A-<right>") 'forward-word)
(global-set-key (kbd "A-<left>") 'backward-word)
(global-set-key (kbd "A-<up>") 'scroll-one-line-down)
(global-set-key (kbd "A-<down>") 'scroll-one-line-up)
(global-set-key (kbd "C-<kp-add>") 'enlarge-window)
(global-set-key (kbd "C-<kp-subtract>") 'shrink-window)
(global-set-key (kbd "C-<kp-enter>") 'other-window)
(global-set-key (kbd "M-<kp-enter>") 'switch-to-buffer)
(global-set-key (kbd "C-<return>") 'other-window)

;; Mouse Button Bindings
(global-set-key (kbd "<mouse-5>") 'next-buffer)
(global-set-key (kbd "<mouse-4>") 'previous-buffer)

(global-set-key (kbd "<mode-line> <mouse-2>") 'transpose-frame)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(global-unset-key (kbd "C-x f"))

(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

(provide 'cc-global-keybindings)

;;; cc-global-keybindings.el ends here
