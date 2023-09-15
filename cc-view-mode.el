;;; cc-view-mode.el 

;;; Commentary:

;;; Code:
(add-hook 'view-mode-hook 'hl-line-mode)

(defun cc/view-exit ()
  (hl-line-mode -1))

(advice-add 'View-exit :after #'cc/view-exit)

(add-hook 'view-mode-hook (lambda ()
                            (cond ((derived-mode-p 'markdown-mode)
                                   (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
                                   (define-key view-mode-map (kbd "n") 'markdown-outline-next))
                                  ((derived-mode-p 'org-mode)
                                   (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
			           (define-key view-mode-map (kbd "p") 'org-next-visible-heading))
                                  ((derived-mode-p 'makefile-mode)
                                   (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
			           (define-key view-mode-map (kbd "n") 'makefile-next-dependency)))))
(provide 'cc-view-mode)
;;; cc-view-mode.el ends here
