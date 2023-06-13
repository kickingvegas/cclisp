;; elfeed-search-mode

(add-hook 'elfeed-search-mode-hook (lambda ()
			             (define-key elfeed-search-mode-map (kbd "/") 'elfeed-search-live-filter)))


