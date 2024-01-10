;; dired-mode
(require 'dired)
(require 'cclisp)

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)


(defun cc/dired-inspect-object ()
  (interactive)
  (if (dired-get-subdir)
      ;; nop
      (message "subdir")
    (let ((fname (dired-get-filename)))
      (cond
       ((not (derived-mode-p 'dired-mode))
        ;; nop
        (message "Not in dired mode."))

       ((file-symlink-p fname)
        ;; nop
        (message fname))

       ((file-directory-p fname)
        ;; open in subdir
        (message "directory")
        (dired-maybe-insert-subdir fname))

       ((file-regular-p fname)
        ;; mark file
        (message fname)
        (dired-find-file))

       (t
        (message "undefined"))))))

;; (add-hook 'dired-mode-hook (lambda ()
;;                              (define-key dired-mode-map (kbd "<mouse-1>") 'cc/dired-inspect-object)))

(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
                             (define-key dired-mode-map (kbd "C-o") 'cc/meta-search)
                             (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
                             (setq-local mouse-1-click-follows-link 'double)
                             ;;(define-key dired-mode-map (kbd "A-M-<mouse-2>") 'cc/dired-inspect-object)
                             (define-key dired-mode-map (kbd "A-M-<mouse-1>") 'browse-url-of-dired-file)))

(provide 'cc-dired-mode)
