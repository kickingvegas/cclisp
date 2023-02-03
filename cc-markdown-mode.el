;; markdown-mode

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)


(add-hook 'markdown-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'company-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
;;(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
(add-hook 'markdown-mode-hook (lambda ()
                                (turn-on-orgtbl)
                                (define-key markdown-mode-map (kbd "C-<up>") 'markdown-backward-same-level)
                                (define-key markdown-mode-map (kbd "C-<down>") 'markdown-forward-same-level)
                                (define-key markdown-mode-map (kbd "M-<f6>") 'markdown-toggle-inline-images)
                                (define-key markdown-mode-map [f13] 'markdown-preview)))


