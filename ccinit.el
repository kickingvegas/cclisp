;;; ccinit.el --- CC Emacs Init File -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Charles Choi

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

;; Configuration when launched via Finder.
(when (or (eq window-system 'mac) (eq window-system 'ns))
  (setenv "PATH" (concat "/Applications/Inkscape.app/Contents/MacOS:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/local/libexec/gnubin:" (getenv "PATH")))
  (setenv "PATH" (concat "/opt/local/lib/ImageMagick7/bin:" (getenv "PATH")))
  (setenv "PATH" (concat (getenv "HOME") "/bin:" (getenv "PATH")))
  (add-to-list 'exec-path "/Applications/Inkscape.app/Contents/MacOS")
  (add-to-list 'exec-path "/opt/local/bin")
  (add-to-list 'exec-path "/opt/local/libexec/gnubin")
  (add-to-list 'exec-path "/opt/local/lib/ImageMagick7/bin")
  (add-to-list 'exec-path (concat (getenv "HOME") "/bin")))

(require 'use-package)
(require 'expand-region)
(require 'wgrep)
(require 'yasnippet)
(require 'pixel-scroll)
(require 'mouse)

(context-menu-mode)

;;(require 'pbcopy)

(cond
 ((eq system-type 'darwin)
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(swift-mode . swift-ts-mode)))

 ((eq system-type 'gnu/linux)
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))))

(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(yas-global-mode 1)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(recentf-mode 1)


;; Config stolen from
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(setq redisplay-skip-fontification-on-input t)

;; (setq read-process-output-max (* 4 1024 1024)) ; 4MB


;; (when (eq window-system 'x)
;;   (setq x-meta-keysym 'super
;; 	x-super-keysym 'meta)
;;   (turn-on-pbcopy))

;;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
;;(setq mouse-wheel-progressive-speed nil)
;;(setq mouse-wheel-follow-mouse 't)
;;(setq scroll-step 1)

;; (when (eq window-system 'mac)
;;     (setq mac-mouse-wheel-mode t)
;;     (setq mac-mouse-wheel-smooth-scroll t))

(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))

;;(setq pixel-scroll-precision-large-scroll-height 10.0)

(when (and (eq window-system 'ns) (boundp 'mac-command-modifier))
  (setq mac-command-modifier 'meta))

(require 'casual-autoload)
(casual-init)
(require 'cclisp)
(require 'cc-ibuffer-mode)
(require 'cc-prog-mode)
(require 'cc-emacs-lisp-mode)
(require 'cc-text-mode)
(require 'cc-org-mode)
;; (require 'cc-org-agenda)
(require 'cc-markdown-mode)
(require 'cc-objc-mode)
(require 'cc-nxml-mode)
(require 'cc-sgml-mode)
;; (require 'cc-bookmarks-bmenu-mode)
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
(if (eq system-type 'darwin)
    (require 'cc-swift-mode))
(require 'flyspell)
(require 'cc-view-mode)
(require 'cc-magit-mode)
(require 'cc-compile-mode)
(require 'cc-grep-mode)
(require 'kill-with-intelligence)
(require 'cc-agenda-timeline)
(require 'cc-truth-table)
(require 'cc-digital-logic)
(require 'cc-package-menu-mode)
(require 'cc-calc-mode)
;;(require 'cc-re-builder)
(require 'cc-symbol-overlay)
;; (require 'cc-calendar-mode)
;; (require 'password-store-menu)
;; (require 'cc-image-mode)
(require 'cc-make-mode)
(require 'cc-csv-mode)
(require 'cc-main-tmenu)
(require 'cc-erc-mode)
(require 'gah)
;;(require 'cc-gnuplot-mode)
(require 'cc-pwa)
(require 'cc-bibtex-mode)
(require 'cc-eww-mode)
(require 'cc-debbugs-mode)
(require 'cc-blog-utils)
;; (require 'cc-css-mode)
;; (require 'cc-html-mode)
(require 'cc-macros)
(require 'ffap)
(require 'calle24)
(require 'scrim-utils)
(require 'numeri)
(require 'casual-agenda)
(require 'cc-menu-reconfig)
(require 'cc-rfc-mode)
(when (eq system-type 'darwin)
  (require 'cc-music))
(require 'anju)
(require 'wttr)
(require 'aqui)
(require 'cc-global-keybindings)
;;; Configure MELPA Packages
;; (require 'casual-isearch)
;; (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)

;; (require 'casual)

;; (casual-init)

;; calle24 config
(when (featurep 'calle24)
  (calle24-refresh-appearance)
  (add-hook 'compilation-mode-hook #'calle24-refresh-appearance))

(use-package hl-line
  :ensure nil
  :defer t
  :hook ((bookmark-bmenu-mode . hl-line-mode)
         (ibuffer-mode . hl-line-mode)))

(anju-init)

;;; Local Customizations

(when (and (string= (system-name) "bingsu.local") (display-graphic-p))
  (server-start)
  (require 'org-protocol))

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

;; (when (eq window-system 'mac)
;;   (mac-toggle-tab-bar))

(defun cc/tty-mouse ()
  "Configure mouse for TTY."
  (interactive)
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") 'scroll-up-line)))

(defun cc/days-until-voting (arg)
  "Days until U.S. elections in 2026 and 2028.

If prefix ARG is non-nil, then the computed result is stored in the
 `kill-ring'."
  (interactive "P")
  (let* ((midterms (cc/--days-until "2026-11-03" "%d days until 2026 midterms"))
         (election (cc/--days-until "2028-11-07" "%d days until 2028 presidential election"))
         (msg (format "%s, %s" midterms election)))
    (if arg
        (kill-new msg))
    (message msg)))

(defun cc/days-until-mothers (arg)
  "Days until Mother's Day 2026.

If prefix ARG is non-nil, then the computed result is stored in the
`kill-ring'."
  (interactive "P")
  (let* ((mother (cc/--days-until "2026-05-10" "%d days until Mother's Day"))
         (msg (format "%s" mother)))
    (if arg
        (kill-new msg))
    (message msg)))

(defun cc/days-until-next-gig (arg)
  "Days until next gig.

If prefix ARG is non-nil, then the computed result is stored in the
`kill-ring'."
  (interactive "P")
  (let* ((event (cc/--days-until "2026-06-07" "%d days until next gig"))
         (msg (format "%s" event)))
    (if arg
        (kill-new msg))
    (message msg)))

(defvar cc--workplace-initialized nil
  "If non-nil then workplace is initialized.")

(defun cc/workplace ()
  "Initialize workplace."
  (interactive)
  (if cc--workplace-initialized
      (message "Workplace already initialized.")

    (if (= (display-pixel-width) 1512)
        (progn
          (cc/--resize-frame 125 49)
          (set-frame-position (selected-frame) 182 44))
      (cc/--resize-frame 141 71)
      (set-frame-position (selected-frame) 852 192))

    (status-report)
    (org-agenda nil "n")
    (casual-agenda-goto-now)
    (eshell t)
    (switch-to-buffer (format-time-string "%Y_%m_%d.org"))
    (setq cc--workplace-initialized t)))

;; Reconfigure gah browse-url
(defun cc/gah-browse-url (&optional issue)
  "Open URL in ISSUE.

Note that UUID in ‘app-id’ is locally defined by macOS. Users must
inspect their local GitHub PWA Info.plist configuration to replace it
accordingly."
  (let* ((issue (if (not issue)
                    (vtable-current-object)
                  issue))
         (url (map-elt issue "url")))
    (cond
     ((or (eq window-system 'ns) (eq window-system 'mac))
      (github url))

     (t
      (browse-url url)))))

(advice-add 'gah-browse-url :override 'cc/gah-browse-url)


(if (and t
         (eq window-system 'ns)
         (string-equal (system-name) "bingsu.local"))
    (add-hook 'window-setup-hook #'cc/workplace))

;; (password-store-menu-enable)

;;(setq window-system-default-frame-alist '((ns . ((ns-transparent-titlebar . t)))))

;; (ffap-bindings)
