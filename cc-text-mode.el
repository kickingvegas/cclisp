;; Text Mode

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'context-menu-mode)
(define-key text-mode-map (kbd "<home>") 'beginning-of-visual-line)
(define-key text-mode-map (kbd "<end>") 'end-of-visual-line)
(define-key text-mode-map (kbd "A-<left>") 'backward-sentence)
(define-key text-mode-map (kbd "A-<right>") 'forward-sentence)
(define-key text-mode-map (kbd "A-M-<left>") 'backward-paragraph)
(define-key text-mode-map (kbd "A-M-<right>") 'forward-paragraph)

;;(setq auto-mode-alist (cons (cons "\\.txt$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.text$" 'text-mode) auto-mode-alist))
;;(setq auto-mode-alist (cons (cons "\\.mml$" 'text-mode) auto-mode-alist))

