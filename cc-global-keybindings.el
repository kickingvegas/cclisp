;; Global Keyboard Mappings
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x v") 'view-file)
(global-set-key (kbd "M-q") 'query-replace)
(global-set-key (kbd "M-j") 'fill-paragraph)
;(global-set-key "\C-h" 'delete-backward-char) (global-set-key "\C-xh"
;'help-for-help) (global-set-key "\r" 'newline-and-indent)

(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "A-<return>") 'other-window)
(global-set-key (kbd "<f3>") 'save-buffers-kill-emacs)
(global-set-key (kbd "<f4>") 'helm-bookmarks)
(global-set-key (kbd "<f5>") 'status-report)
(global-set-key (kbd "S-<f5>") 'cc/select-journal-file)
(global-set-key (kbd "<f6>") 'osx-dictionary-search-input)
(global-set-key (kbd "<f7>") 'call-last-kbd-macro)
(global-set-key (kbd "<f8>") 'indent-region)
(global-set-key (kbd "<f9>") 'compile)
(global-set-key (kbd "<f11>") 'bookmark-set)

(global-set-key (kbd "M-<f1>") 'cc/open-url)
(global-set-key (kbd "M-<f2>") 'google-this)
(global-set-key (kbd "C-c C-;") 'shell-command)
(global-set-key (kbd "M-<f4>") 'helm-buffers-list)

(global-set-key (kbd "<f13>") 'neotree-toggle)
(global-set-key (kbd "M-<f13>") 'treemacs)
(global-set-key (kbd "<f14>") 'eshell)
(global-set-key (kbd "<f15>") 'ediff-revision)
(global-set-key (kbd "<f16>") 'calc)

;; Terminal 
(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-c") 'kill-ring-save)

;;(global-set-key (kbd "<f8>") 'next-error)
;;(global-set-key (kbd "M-<f8>") 'previous-error)
;;(global-set-key (kbd "<f9>") 'compile)

(global-set-key (kbd "C-<f2>") 'delete-other-windows)
(global-set-key (kbd "C-<f3>") 'kill-buffer)
(global-set-key (kbd "C-<f4>") 'view-file)
(global-set-key (kbd "C-z") 'pop-global-mark)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "S-<home>") 'back-to-indentation)
(global-set-key (kbd "S-<end>") 'end-of-line)
(global-set-key (kbd "<PageDown>") 'scroll-down-command)
(global-set-key (kbd "<PageUp>") 'scroll-up-command)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "<delete>") 'delete-forward-char)

(when (string-equal (window-system) "mac")
  (global-set-key (kbd "C-<tab>") 'mac-next-tab)
  ;; this binding breaks terminal behavior
  ;(global-set-key (kbd "M-]") 'mac-next-tab)
  ;(global-set-key (kbd "M-[") 'mac-previous-tab)    
  (define-key-after global-map
    [menu-bar file mac-toggle]
    '("Toggle Tab Bar" . mac-toggle-tab-bar) 'close-tab))

;; Left Side Keys
;; (global-set-key (kbd "<f11>") 'shell)
;; (global-set-key (kbd "C-<f11>")  'shell-new)

;; Keypad Keys
(global-set-key (kbd "M-<right>") 'forward-word)
(global-set-key (kbd "M-<left>") 'backward-word)
(global-set-key (kbd "A-<right>") 'forward-word)
(global-set-key (kbd "A-<left>") 'backward-word)
(global-set-key (kbd "A-<up>") 'scroll-one-line-down)
(global-set-key (kbd "A-<down>") 'scroll-one-line-up)
(global-set-key (kbd "C-<kp-add>") 'enlarge-window)
(global-set-key (kbd "C-<kp-subtract>") 'shrink-window)
(global-set-key (kbd "C-<kp-enter>") 'other-window)
(global-set-key (kbd "M-<kp-enter>") 'switch-to-buffer)
(global-set-key (kbd "C-<return>") 'other-window)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(global-unset-key (kbd "C-x f"))

(define-key global-map
  [menu-bar tools find-name]
  '("Find File…" . find-name-dired))

(define-key global-map
  [menu-bar tools open-in-finder]
  '("Open in Finder" . reveal-in-folder-this-buffer))

(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)
