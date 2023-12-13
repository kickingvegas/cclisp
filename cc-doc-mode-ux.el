;;; cc-doc-mode-ux.el --- Documentation Mode UX Modifications by cchoi

;;; Commentary:
;; UX modifications for different Emacs documentation modes.
;; Covers Info, Help, Man, and Shortdoc.

;;; Code:
(require 'info)
(require 'help)
(require 'shortdoc)
(require 'man)
(require 'hl-line)

(defun cc/docview-backward-paragraph ()
  "Move point backward paragraph such that the first line is highlighted.

This function is intended to be used with `hl-line-mode'."
  (interactive)
  (backward-paragraph 2)
  (forward-line))

(defun cc/docview-forward-paragraph ()
  "Move point forward paragraph such that the first line is highlighted.

This function is intended to be used with `hl-line-mode'."
  (interactive)
  (forward-paragraph)
  (forward-line))

;; Info
(add-hook 'Info-mode-hook
          (lambda ()
            ;; Use web-browser history navigation bindings
            (define-key Info-mode-map (kbd "M-[") 'Info-history-back)
            (define-key Info-mode-map (kbd "M-]") 'Info-history-forward)
            ;; Bind p and n to paragraph navigation
            (define-key Info-mode-map (kbd "p") 'cc/docview-backward-paragraph)
            (define-key Info-mode-map (kbd "n") 'cc/docview-forward-paragraph)
            ;; Bind <f1> to help
            (define-key Info-mode-map (kbd "<f1>") 'Info-help)
            ;; Bind M-j, M-k to scrolling up/down line
            (define-key Info-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key Info-mode-map (kbd "M-k") 'scroll-down-line)
            ;; Bind h and l to navigate to previous and next nodes
            ;; Bind j and k to navigate to next and previous references
            (define-key Info-mode-map (kbd "h") 'Info-prev)
            (define-key Info-mode-map (kbd "j") 'Info-next-reference)
            (define-key Info-mode-map (kbd "k") 'Info-prev-reference)
            (define-key Info-mode-map (kbd "l") 'Info-next)
            ;; Bind / to search
            (define-key Info-mode-map (kbd "/") 'Info-search)
            ;; Set Bookmark
            (define-key Info-mode-map (kbd "B") 'bookmark-set)
            ;; Bind side mouse buttons on Logitech mouse
            (define-key Info-mode-map (kbd "<mouse-5>") 'Info-history-forward)
            (define-key Info-mode-map (kbd "<mouse-4>") 'Info-history-back)))

(add-hook 'Info-mode-hook 'hl-line-mode)

;; Help
(add-hook 'help-mode-hook
          (lambda ()
            ;; Use web-browser history navigation bindings
            (define-key help-mode-map (kbd "M-[") 'help-go-back)
            (define-key help-mode-map (kbd "M-]") 'help-go-forward)
            ;; Bind p and n to paragraph navigation
            (define-key help-mode-map (kbd "p") 'cc/docview-backward-paragraph)
            (define-key help-mode-map (kbd "n") 'cc/docview-forward-paragraph)
            ;; Bind <f1> to help
            (define-key help-mode-map (kbd "<f1>") 'describe-mode)
            ;; Bind M-j, M-k to scrolling up/down line
            (define-key help-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key help-mode-map (kbd "M-k") 'scroll-down-line)
            ;; Bind j and k to navigate to forward and backward buttons
            (define-key help-mode-map (kbd "j") 'forward-button)
            (define-key help-mode-map (kbd "k") 'backward-button)
            ;; Bind side mouse buttons on Logitech mouse
            (define-key help-mode-map (kbd "<mouse-5>") 'help-go-forward)
            (define-key help-mode-map (kbd "<mouse-4>") 'help-go-back)))

(add-hook 'help-mode-hook 'hl-line-mode)

;; Shortdoc
(add-hook 'shortdoc-mode-hook
          (lambda ()
            ;; Bind <f1> to help
            (define-key shortdoc-mode-map (kbd "<f1>") 'describe-mode)
            ;; Bind M-j, M-k to scrolling up/down line
            (define-key shortdoc-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key shortdoc-mode-map (kbd "M-k") 'scroll-down-line)
            ;; Bind h and l to navigate to previous and next sections
            ;; Bind j and k to navigate to next and previous
            (define-key shortdoc-mode-map (kbd "h") 'shortdoc-previous-section)
            (define-key shortdoc-mode-map (kbd "j") 'shortdoc-next)
            (define-key shortdoc-mode-map (kbd "k") 'shortdoc-previous)
            (define-key shortdoc-mode-map (kbd "l") 'shortdoc-next-section)))

(add-hook 'shortdoc-mode-hook 'hl-line-mode)

;; Man
(add-hook 'Man-mode-hook
          (lambda ()
            ;; Bind <f1> to help
            (define-key Man-mode-map (kbd "<f1>") 'describe-mode)
            ;; Bind M-j, M-k to scrolling up/down line
            (define-key Man-mode-map (kbd "M-j") 'scroll-up-line)
            (define-key Man-mode-map (kbd "M-k") 'scroll-down-line)
            ;; Bind j and k to navigate forward and backward paragraphs
            (define-key Man-mode-map (kbd "j") 'cc/docview-forward-paragraph)
            (define-key Man-mode-map (kbd "k") 'cc/docview-backward-paragraph)
            ;; Bind K to kill buffer to replace override of default k above
            (define-key Man-mode-map (kbd "K") 'Man-kill)))

(add-hook 'Man-mode-hook 'hl-line-mode)

(provide 'cc-doc-mode-ux)

;;; cc-doc-mode-ux.el ends here
