;; Shell Preferences 
(setq shell-mode-hook
      '((lambda ()
	  (font-lock-mode 1)
          (context-menu-mode 1)
	  (define-key shell-mode-map "\C-p" 'comint-previous-input)
	  (define-key shell-mode-map "\C-n" 'comint-next-input))))

