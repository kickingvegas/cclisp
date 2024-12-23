;;; ccinit.el --- CC Emacs Init File -*- lexical-binding: t; -*-

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
;; Charles Choi Emacs Initialization File

;;; Code:
(setenv "CDPATH" ".:..:~")

(when (or (eq window-system 'mac) (eq window-system 'ns))
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/local/libexec/gnubin:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/local/lib/ImageMagick7/bin:" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/opt/local/bin")
  (add-to-list 'exec-path "/opt/local/libexec/gnubin")
  (add-to-list 'exec-path "/opt/local/lib/ImageMagick7/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/bin")))

(require 'use-package)
(require 'expand-region)
(require 'wgrep)
(require 'yasnippet)
;;(require 'pbcopy)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(yas-global-mode 1)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(recentf-mode 1)

;; (when (eq window-system 'x)
;;   (setq x-meta-keysym 'super
;; 	x-super-keysym 'meta)
;;   (turn-on-pbcopy))

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;(setq mouse-wheel-progressive-speed nil)
;;(setq mouse-wheel-follow-mouse 't)
;;(setq scroll-step 1)

(when (eq window-system 'mac)
    (setq mac-mouse-wheel-mode t)
    (setq mac-mouse-wheel-smooth-scroll t))

;;(setq pixel-scroll-precision-large-scroll-height 10.0)

(when (eq window-system 'ns)
  (setq mac-command-modifier 'meta))

;;(require 'avy)
(require 'cclisp)
(require 'cc-ibuffer-mode)
(require 'cc-prog-mode)
(require 'cc-emacs-lisp-mode)
(require 'cc-text-mode)
(require 'cc-org-mode)
(require 'cc-org-agenda)
(require 'cc-markdown-mode)
(require 'cc-objc-mode)
(require 'cc-nxml-mode)
(require 'cc-sgml-mode)
(require 'cc-bookmarks-bmenu-mode)
(require 'cc-dired-mode)
(require 'cc-js-mode)
(require 'cc-tetris-mode)
(require 'cc-eshell-mode)
(require 'cc-shell-mode)
(require 'cc-elfeed-mode)
(require 'cc-google-translate)
(require 'cc-repeat-mode)
(require 'cc-doc-mode-ux)
(require 'cc-info-mode)
(require 'cc-ediff-mode)
(require 'cc-occur-mode)
(require 'cc-context-menu)
(require 'cc-diff-hl-mode)
(require 'cc-python-mode)
(require 'cc-swift-mode)
(require 'flyspell)
(require 'cc-view-mode)
(require 'cc-global-keybindings)
(require 'cc-magit-mode)
(require 'cc-menu-reconfig)
(require 'cc-grep-mode)
(require 'kill-with-intelligence)
(require 'cc-agenda-timeline)
(require 'cc-truth-table)
(require 'cc-digital-logic)
(require 'cc-package-menu-mode)
(require 'cc-calc-mode)
(require 'cc-re-builder)
(require 'cc-symbol-overlay)
(require 'cc-calendar-mode)
(require 'password-store-menu)
(require 'cc-image-mode)
(require 'cc-make-mode)

;;; Configure MELPA Packages
(require 'casual-isearch)
(keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)

(use-package hl-line
  :ensure nil
  :defer t
  :hook ((bookmark-bmenu-mode . hl-line-mode)
         (ibuffer-mode . hl-line-mode)))

;;; Local Customizations

(when (and (string= (system-name) "bingsu.local") (display-graphic-p))
  (server-start))

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;; Miscellaneous Stuff
(fset 'yes-or-no-p 'y-or-n-p)           ; set yes-or-no to y-or-n

;; (setq tab-bar-mode-hook
;;       '((lambda ()
;;           (if (display-graphic-p)
;;               (progn
;; 	        (local-set-key (kbd "M-]") 'tab-bar-switch-to-next-tab)
;; 	        (local-set-key (kbd "M-[") 'tab-bar-switch-to-prev-tab)))
;; 	  )))

(add-to-list 'auto-mode-alist '("\\.msc\\'" . graphviz-dot-mode))
(add-to-list 'auto-mode-alist '("\\.xcconfig\\'" . conf-mode))

(when (eq window-system 'mac)
  (mac-toggle-tab-bar))

(defun cc/tty-mouse ()
  (interactive)
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

(password-store-menu-enable)
