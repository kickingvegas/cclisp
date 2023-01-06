;; Charles Choi Emacs Initialization File

(require 'use-package)
(require 'expand-region)
(require 'wgrep)
(require 'yasnippet)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)

(yas-global-mode 1)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
(recentf-mode 1)

(when (eq window-system 'x)
  (require 'pbcopy)
  (turn-on-pbcopy))

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq mac-mouse-wheel-mode t)
(setq mac-mouse-wheel-smooth-scroll t)

(load "cclisp")
(load "cc-global-keybindings")
(load "cc-prog-mode")
(load "cc-text-mode")
(load "cc-org-mode")
(load "cc-markdown-mode")
(load "cc-objc-mode")
(load "cc-dired-mode")
(load "cc-js-mode")
(load "cc-tetris-mode")
(load "cc-eshell-mode")

(when (string= (system-name) "bingsu.local")
  (server-start))

