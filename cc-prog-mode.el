(setq prog-mode-hook
      '((lambda ()
          (local-set-key (kbd "<home>") 'back-to-indentation)
	  (display-line-numbers-mode 1)
          (electric-pair-mode 1)
          (company-mode 1)
          (context-menu-mode 1)
          (rainbow-mode 1)
          (display-fill-column-indicator-mode 1)
	  (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
	  )))

;; GUD - mode preferences
(setq gud-mode-hook
      '((lambda ()
	  (local-set-key [f7] 'gud-step)
	  (local-set-key [f8] 'gud-next)
	  (local-set-key [f9] 'gud-cont))))
