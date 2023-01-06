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
                            'face `(:foreground "blue"))
                "\n┗━━ "
                (if (= (user-uid) 0) "# " "$ "))))

(add-hook 'eshell-mode-hook (lambda ()
                              (push "gdu-go" eshell-visual-commands)))
