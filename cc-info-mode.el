;; Info-mode

(require 'info)
(require 'help)
(require 'shortdoc)
(require 'man)

(defun cc/docview-backward-paragraph ()
  (interactive)
  (backward-paragraph 2)
  (forward-line))

(defun cc/docview-forward-paragraph ()
  (interactive)
  (forward-paragraph)
  (forward-line))

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map (kbd "M-[") 'Info-history-back)
            (define-key Info-mode-map (kbd "M-]") 'Info-history-forward)
            (define-key Info-mode-map (kbd "p") 'cc/docview-backward-paragraph)
            (define-key Info-mode-map (kbd "n") 'cc/docview-forward-paragraph)
            (define-key Info-mode-map (kbd "<f1>") 'Info-help)
            (define-key Info-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key Info-mode-map (kbd "M-k") 'scroll-down-line)
            (define-key Info-mode-map (kbd "h") 'Info-prev)
            (define-key Info-mode-map (kbd "j") 'Info-next-reference)
            (define-key Info-mode-map (kbd "k") 'Info-prev-reference)
            (define-key Info-mode-map (kbd "l") 'Info-next)
            (define-key Info-mode-map (kbd "/") 'Info-search)
            (define-key Info-mode-map (kbd "<mouse-5>") 'Info-history-forward)
            (define-key Info-mode-map (kbd "<mouse-4>") 'Info-history-back)))

(add-hook 'Info-mode-hook 'hl-line-mode)


(add-hook 'help-mode-hook
          (lambda ()
            (define-key help-mode-map (kbd "M-[") 'help-go-back)
            (define-key help-mode-map (kbd "M-]") 'help-go-forward)
            (define-key help-mode-map (kbd "p") 'cc/docview-backward-paragraph)
            (define-key help-mode-map (kbd "n") 'cc/docview-forward-paragraph)
            (define-key help-mode-map (kbd "<f1>") 'describe-mode)
            (define-key help-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key help-mode-map (kbd "M-k") 'scroll-down-line)
            (define-key help-mode-map (kbd "j") 'forward-button)
            (define-key help-mode-map (kbd "k") 'backward-button)
            (define-key help-mode-map (kbd "<mouse-5>") 'help-go-forward)
            (define-key help-mode-map (kbd "<mouse-4>") 'help-go-back)))

(add-hook 'help-mode-hook 'hl-line-mode)

(add-hook 'shortdoc-mode-hook
          (lambda ()
            (define-key shortdoc-mode-map (kbd "h") 'shortdoc-previous-section)
            (define-key shortdoc-mode-map (kbd "j") 'shortdoc-next)
            (define-key shortdoc-mode-map (kbd "k") 'shortdoc-previous)
            (define-key shortdoc-mode-map (kbd "l") 'shortdoc-next-section)))

(add-hook 'shortdoc-mode-hook 'hl-line-mode)

(add-hook 'Man-mode-hook
          (lambda ()
            (define-key Man-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key Man-mode-map (kbd "M-k") 'scroll-down-line)
            (define-key Man-mode-map (kbd "j") 'cc/docview-forward-paragraph)
            (define-key Man-mode-map (kbd "k") 'cc/docview-backward-paragraph)
            (define-key Man-mode-map (kbd "K") 'Man-kill)))

(add-hook 'Man-mode-hook 'hl-line-mode)

(provide 'cc-info-mode)
