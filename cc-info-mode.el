;; Info-mode

(add-hook 'Info-mode-hook
          (lambda ()
            (define-key Info-mode-map (kbd "M-[") 'Info-history-back)
            (define-key Info-mode-map (kbd "M-]") 'Info-history-forward)
            (define-key Info-mode-map (kbd "<mouse-5>") 'Info-history-forward)
            (define-key Info-mode-map (kbd "<mouse-4>") 'Info-history-back)))

(provide 'cc-info-mode)