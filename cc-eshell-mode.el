;; eshell-mode

(setq eshell-prompt-regexp "┗━━ \\$ "
      eshell-prompt-function
      (lambda nil
        (concat "\n┏━ "
                (user-login-name) "@" (system-name) ":"
                (propertize (if (string= (eshell/pwd) (getenv "HOME"))
                                "~"
                              (replace-regexp-in-string
                               (concat "^" (getenv "HOME")) "~" (eshell/pwd)))
                            'face `(:foreground "orange red"))
                "\n┗━━ "
                (if (= (user-uid) 0) "# " "$ "))))

(add-hook 'eshell-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook (lambda ()
			      (define-key eshell-mode-map (kbd "<tab>") 'company-complete)
			      (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history)
			      (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
			      (add-to-list 'eshell-visual-options '("gh" "help"))
			      (add-to-list 'eshell-visual-options '("swift" "repl"))                              
			      (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
                              (push "gdu-go" eshell-visual-commands)
                              (push "gh" eshell-visual-commands)))

(provide 'cc-eshell-mode)
