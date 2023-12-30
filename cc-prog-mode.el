;;; cc-prog-mode.el --- Programming Customizations
;; prog-mode

;;; Commentary:
;;
(require 'prog-mode)
(require 'company)
(require 'cc-save-hooks)
(require 'rainbow-mode)
(require 'display-line-numbers)
(require 'display-fill-column-indicator)
(require 'hl-line)

;;; Code:

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'context-menu-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook #'cc/save-hook-delete-trailing-whitespace)

(define-key prog-mode-map [remap indent-for-tab-command]
  #'company-indent-or-complete-common)

(define-key prog-mode-map (kbd "C-a") 'back-to-indentation)

;; GUD - mode preferences
(setq gud-mode-hook
      '((lambda ()
	  (local-set-key [f7] 'gud-step)
	  (local-set-key [f8] 'gud-next)
	  (local-set-key [f9] 'gud-cont))))

(add-hook 'makefile-mode-hook (lambda ()
                                (local-set-key (kbd "<f9>") 'compile)))

(provide 'cc-prog-mode)
;;; cc-prog-mode.el ends here
