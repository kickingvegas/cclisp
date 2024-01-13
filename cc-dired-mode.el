;;; cc-dired-mode.el --- Dired Customization -*- lexical-binding: t -*-
;; dired-mode

;;; Commentary:
;;

;;; Code:
(require 'dired)
(require 'cclisp)
(require 'helm)
(require 'org)
(require 'wdired)
(require 'cc-dired-sort-by)

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)

(defun cc/dired-image-info ()
  "Message image info in the minibuffer and push into kill-ring."
  (interactive)
  (when (org-file-image-p (dired-get-filename))
    (let* ((filename (dired-get-filename))
           (image-info (cc/--image-info filename))
           (output (concat image-info
                           " "
                           (file-name-base filename)
                           "."
                           (file-name-extension filename))))
      (message output)
      (kill-new output))))

(defun cc/dired-inspect-object ()
  "WIP: inspect Dired object."
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

(add-hook
 'dired-mode-hook
 (lambda ()
   (setq-local mouse-1-click-follows-link 'double)))

(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "C-o") 'cc/meta-search)
(define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "s") 'cc/dired-sort-by)
(define-key dired-mode-map (kbd "j") 'helm-find-files)
(define-key dired-mode-map (kbd "M-n") 'dired-next-subdir)
(define-key dired-mode-map (kbd "M-p") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "A-M-<mouse-1>") 'browse-url-of-dired-file)
;;(define-key dired-mode-map (kbd "A-M-<mouse-2>") 'cc/dired-inspect-object)

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
