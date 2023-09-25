;;; cc-menu-reconfig.el --- Menu reconfiguration

;;; Commentary:
;;

;; Menu Settings
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'text-mode)

(easy-menu-add-item (lookup-key global-map [menu-bar file]) nil
                    ["Swap Windows"
                     window-swap-states
                     :visible (> (count-windows) 1)
                     :help "Swap the states of live windows WINDOW-1 and \
WINDOW-2."]
                    "New Window Below")

;;; Reconfigure Text Mode Menu

(easy-menu-remove-item text-mode-menu nil "Center Line")
(easy-menu-remove-item text-mode-menu nil "Center Region")
(easy-menu-remove-item text-mode-menu nil "Center Paragraph")
(easy-menu-remove-item text-mode-menu nil "Paragraph Indent")
(easy-menu-remove-item text-mode-menu nil "---")

(easy-menu-add-item text-mode-menu nil cc/transform-text-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil cc/emphasize-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil cc/region-operations-menu "Auto Fill")

;;; Reconfigure Edit Menu

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/transpose-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/move-text-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/delete-space-menu "Fill")

;;; Reconfigure Tools Menu

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Agenda - All TODOs"
                     (lambda () (interactive)(org-agenda nil "n"))
                     :help "Show Org agenda with all TODO tasks."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Open in Finder"
                     reveal-in-folder-this-buffer
                     :help "Reveal the current buffer in folder."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["---" separator-org]
                    "Open in Finder")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find File…"
                     find-name-dired
                     :help "Search DIR recursively for files matching the \
globbing PATTERN, and run Dired on those files."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find in Files (rgrep)…"
                     rgrep
                     :help "Recursively grep for REGEXP in FILES in directory \
tree rooted at DIR."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Search Org Notes…"
                     cc/org-search
                     :help "Search Org Notes in ~/org."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["----" separator-shell]
                    "Shell Commands")


(define-key global-map [menu-bar tools grep] nil)
(define-key global-map [menu-bar tools rgrep] nil)
(define-key global-map [menu-bar tools ede] nil)
(define-key global-map [menu-bar tools semantic] nil)
(define-key global-map [menu-bar tools compile] nil)
(define-key global-map [menu-bar tools gdb] nil)
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools directory-search] nil)
(define-key global-map [menu-bar tools browse-web] nil)
(define-key global-map [menu-bar tools separator-net] nil)
(define-key global-map [menu-bar tools encryption-decryption] nil)
(define-key global-map [menu-bar tools separator-encryption-decryption] nil)
(define-key global-map [menu-bar tools Table] nil)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (cond ((string= (derived-mode-p 'dired-mode)
                                              "dired-mode")
                                     (file-directory-p ".git"))
                                    ((bound-and-true-p buffer-file-name)
                                     (vc-registered (buffer-file-name))))
                     :help "Show the status of the current Git repository \
in a buffer"]
                    "Version Control")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Count Words"
                     count-words
                     :help "Count words in buffer or region if active."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Eshell"
                     eshell
                     :help "Create an interactive Eshell buffer."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Python Shell"
                     run-python
                     :help "Run an inferior Python process."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["RE-Builder"
                     re-builder
                     :help "Construct a regexp interactively."]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["-----" separator-re]
                    "Calendar")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["World Clock"
                     world-clock
                     :help "Display a world clock buffer with times in \
various time zones."]
                    "Programmable Calculator")

;; Streamlined Bookmarks Menu
(easy-menu-define cc/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu"
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    ["--" nil]
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    ["---" nil]
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(easy-menu-add-item global-map '(menu-bar)
                    cc/bookmarks-menu
                    "Tools")

(define-key global-map [menu-bar edit bookmark] nil)

(provide 'cc-menu-reconfig)

;;; cc-menu-reconfig.el ends here
