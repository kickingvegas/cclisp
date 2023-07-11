;;; cc-diff-hl-mode.el --- diff-hl-mode configuration

;;; Commentary:
;; 
(require 'diff-hl)

(global-diff-hl-mode)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'cc-diff-hl-mode)

;;; cc-diff-hl-mode.el ends here
