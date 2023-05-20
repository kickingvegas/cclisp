;; emacs-lisp-mode

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
                                  (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))
