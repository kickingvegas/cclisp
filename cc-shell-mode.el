;; Shell Preferences 

(add-hook 'shell-mode-hook 'context-menu-mode)
(define-key shell-mode-map "\C-p" 'comint-previous-input)
(define-key shell-mode-map "\C-n" 'comint-next-input)
