;;; cc-view-mode.el 

;;; Commentary:

;;; Code:

(require 'view)

(add-hook 'view-mode-hook 'hl-line-mode)

(defun cc/view-exit ()
  (hl-line-mode -1))

(advice-add 'View-exit :after #'cc/view-exit)

(add-hook
 'view-mode-hook
 (lambda ()
   (cond ((derived-mode-p 'org-mode)
          (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
	  (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
         ((derived-mode-p 'markdown-mode)
          (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
          (define-key view-mode-map (kbd "n") 'markdown-outline-next))
         ((derived-mode-p 'html-mode)
          (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
	  (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
         ((derived-mode-p 'python-mode)
          (define-key view-mode-map (kbd "p") 'python-nav-backward-block)
	  (define-key view-mode-map (kbd "n") 'python-nav-forward-block))
         ((derived-mode-p 'emacs-lisp-mode)
          (define-key view-mode-map (kbd "p") 'backward-sexp)
	  (define-key view-mode-map (kbd "n") 'forward-sexp))
         ((derived-mode-p 'makefile-mode)
          (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
	  (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
         ((derived-mode-p 'c-mode)
          (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
          (define-key view-mode-map (kbd "n") 'c-end-of-defun))
         (t
          (define-key view-mode-map (kbd "p") 'scroll-down-command)
          (define-key view-mode-map (kbd "n") 'scroll-up-command)))))

(provide 'cc-view-mode)
;;; cc-view-mode.el ends here
