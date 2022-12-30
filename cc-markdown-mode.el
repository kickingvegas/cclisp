(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(setq markdown-mode-hook
      '((lambda ()
          (local-set-key (kbd "C-<up>") 'markdown-backward-same-level)
          (local-set-key (kbd "C-<down>") 'markdown-forward-same-level)
          (local-set-key (kbd "M-<f6>") 'markdown-toggle-inline-images)
          (local-set-key [f13] 'markdown-preview)
	  (variable-pitch-mode 1)
          (company-mode 1)
	  (flyspell-mode 1)
          (context-menu-mode 1)
          (turn-on-orgtbl)
          (markdown-toggle-markup-hiding 1)
	  )))

(add-to-list 'auto-mode-alist '("\\.txt\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;(add-hook 'markdown-mode-hook 'turn-on-orgtbl)




