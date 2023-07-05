;; dired-mode
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook (lambda ()
                             (load "dired-x")))

(provide 'cc-dired-mode)
