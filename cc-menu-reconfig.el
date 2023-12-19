;;; cc-menu-reconfig.el --- Menu reconfiguration

;;; Commentary:
;;

;; Menu Settings
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'text-mode)
(require 'vc)
(require 'helm)

(easy-menu-add-item (lookup-key global-map [menu-bar file]) nil
                    ["Swap Windows"
                     window-swap-states
                     :visible (> (count-windows) 1)
                     :help "Swap the states of live windows WINDOW-1 and \
WINDOW-2."]
                    "New Window Below")

(easy-menu-add-item (lookup-key global-map [menu-bar file]) nil
                    ["Transpose Windows"
                     transpose-frame
                     :visible (> (count-windows) 1)
                     :help "Transpose windows arrangement at FRAME."]
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

(easy-menu-add-item global-map '(menu-bar edit)
                    ["Flush Lines…"
                     flush-lines
                     :help "Delete lines containing matches for REGEXP."
                     :visible (not buffer-read-only)]
                    "Fill")

(easy-menu-add-item global-map '(menu-bar edit)
                    ["Keep Lines…"
                     keep-lines
                     :help "Delete all lines except those containing matches \
for REGEXP."
                     :visible (not buffer-read-only)]
                    "Fill")

;;; Reconfigure Tools Menu

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Agenda - All TODOs"
                     (lambda () (interactive)(org-agenda nil "n"))
                     :help "Show Org agenda with all TODO tasks."]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Set Input Method - Korean"
                     (lambda () (interactive)(set-input-method 'korean-hangul))
                     :enable (not current-input-method)
                     :help "Set input method to Korean"]
                    "Shell Commands")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Open in Finder"
                     reveal-in-folder-this-buffer
                     :visible (or (buffer-file-name) (derived-mode-p 'dired-mode))
                     :help "Reveal the current buffer in folder."]
                    "Shell Commands")

(keymap-set-after (lookup-key global-map [menu-bar tools])
  "<separator-org>"
  '(menu-item "--")
  'Agenda\ -\ All\ TODOs)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Find File…"
                     helm-find-files
                     :help "Fuzzy find file."]
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
                    ["IELM"
                     ielm
                     :help "Interactively evaluate Emacs Lisp expressions."]
                     "Language Server Support (Eglot)")

(keymap-set-after (lookup-key global-map [menu-bar tools])
  "<separator-shell>"
  '(menu-item "--")
  'Search\ Org\ Notes…)

(define-key global-map [menu-bar tools grep] nil t)
(define-key global-map [menu-bar tools rgrep] nil t)
(define-key global-map [menu-bar tools ede] nil t)
(define-key global-map [menu-bar tools semantic] nil t)
(define-key global-map [menu-bar tools compile] nil t)
(define-key global-map [menu-bar tools gdb] nil t)
(define-key global-map [menu-bar tools gnus] nil t)
(define-key global-map [menu-bar tools rmail] nil t)
(define-key global-map [menu-bar tools project] nil t)
(define-key global-map [menu-bar tools compose-mail] nil t)
(define-key global-map [menu-bar tools directory-search] nil t)
(define-key global-map [menu-bar tools browse-web] nil t)
(define-key global-map [menu-bar tools separator-net] nil t)
(define-key global-map [menu-bar tools encryption-decryption] nil t)
(define-key global-map [menu-bar tools separator-encryption-decryption] nil t)
(define-key global-map [menu-bar tools Table] nil t)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (vc-responsible-backend default-directory t)
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

(keymap-set-after (lookup-key global-map [menu-bar tools])
  "<separator-re>"
  '(menu-item "--")
  'RE-Builder)

(easy-menu-add-item global-map '(menu-bar tools)
                    ["World Clock"
                     world-clock
                     :help "Display a world clock buffer with times in \
various time zones."]
                    "Programmable Calculator")

(easy-menu-add-item global-map '(menu-bar tools)
                    ["Babel Ingest - Org Table To SQL"
                     (org-babel-lob-ingest "~/org/babel/cc-org-table-to-sql.org")
                     :help "Ingest code block to convert Org Table to SQLite."]
                    "Games")

(keymap-set-after (lookup-key global-map [menu-bar tools])
  "<separator-babel>"
  '(menu-item "--")
  'Babel\ Ingest\ -\ Org\ Table\ To\ SQL)

;; Streamlined Bookmarks Menu
(easy-menu-define cc/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu"
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    "---"
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    "---" 
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(easy-menu-add-item global-map '(menu-bar)
                    cc/bookmarks-menu
                    "Tools")

(define-key global-map [menu-bar edit bookmark] nil t)

(provide 'cc-menu-reconfig)

;;; cc-menu-reconfig.el ends here
