;; Text Mode
(setq text-mode-hook
      '((lambda ()
          (local-set-key (kbd "<home>") 'beginning-of-visual-line)
          (local-set-key (kbd "<end>") 'end-of-visual-line)
          (local-set-key (kbd "A-<left>") 'backward-sentence)
          (local-set-key (kbd "A-<right>") 'forward-sentence)
          (local-set-key (kbd "A-M-<left>") 'backward-paragraph)
          (local-set-key (kbd "A-M-<right>") 'forward-paragraph)
	  (visual-line-mode 1)
          (context-menu-mode 1)
	  (define-key text-mode-map "\es" 'search-forward))))

;;(setq auto-mode-alist (cons (cons "\\.txt$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.text$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.mml$" 'text-mode) auto-mode-alist))

