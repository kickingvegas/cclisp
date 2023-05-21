;; Charles Choi Emacs Initialization File

(setenv "CDPATH" ".:..:~")

(when (eq window-system 'mac)
  (setenv "PATH" (concat "/opt/local/libexec/gnubin:" (getenv "PATH")))
  (setq exec-path (push '"/opt/local/libexec/gnubin" exec-path)))

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
(load "cc-emacs-lisp-mode")
(load "cc-text-mode")
(load "cc-org-mode")
(load "cc-markdown-mode")
(load "cc-objc-mode")
(load "cc-dired-mode")
(load "cc-js-mode")
(load "cc-tetris-mode")
(load "cc-eshell-mode")
(load "cc-google-translate")
(load "cc-appearance")

(when (string= (system-name) "bingsu.local")
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
