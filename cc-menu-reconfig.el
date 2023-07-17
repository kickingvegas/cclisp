;;; cc-menu-reconfig.el --- Menu reconfiguration

;;; Commentary:
;;

;; Menu Settings
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'text-mode)

(define-key global-map
  [menu-bar tools find-in-files]
  '("Find in Files (rgrep)…" . rgrep))

(define-key global-map
  [menu-bar tools find-name]
  '("Find File…" . find-name-dired))

(define-key global-map
  [menu-bar tools open-in-finder]
  '("Open in Finder" . reveal-in-folder-this-buffer))

(global-set-key [vertical-scroll-bar down-mouse-1] 'scroll-bar-drag)

;;; Reconfigure Text Mode Menu

(easy-menu-remove-item text-mode-menu nil "Center Line")
(easy-menu-remove-item text-mode-menu nil "Center Region")
(easy-menu-remove-item text-mode-menu nil "Center Paragraph")
(easy-menu-remove-item text-mode-menu nil "Paragraph Indent")
(easy-menu-remove-item text-mode-menu nil "---")

(easy-menu-add-item text-mode-menu nil cc/transform-text-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil cc/emphasize-menu "Auto Fill")
(easy-menu-add-item text-mode-menu nil cc/region-operations-menu "Auto Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    cc/transpose-menu "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    ["Join Line"
                     join-line
                     :help "Join this line to previous and fix up \
whitespace at join"]
                    "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    ["Delete Horizontal Space"
                     delete-horizontal-space
                     :help "Delete all spaces and tabs around point."]
                    "Fill")

(easy-menu-add-item (lookup-key global-map [menu-bar edit]) nil
                    ["Delete Blank Lines"
                     delete-blank-lines
                     :help "On blank line, delete all surrounding blank lines, \
leaving just one."]
                    "Fill")

;;; Reconfigure Tools Menu
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

(easy-menu-add-item (lookup-key global-map [menu-bar tools]) nil
                    ["Eshell"
                     eshell
                     :help "Create an interactive Eshell buffer."]
                    "Calendar")

(easy-menu-add-item (lookup-key global-map [menu-bar tools]) nil
                    ["Python Shell"
                     run-python
                     :help "Run an inferior Python process."]
                    "Calendar")

(easy-menu-add-item (lookup-key global-map [menu-bar tools]) nil
                    ["RE-Builder"
                     re-builder
                     :help "Construct a regexp interactively."]
                    "Calendar")

(easy-menu-add-item (lookup-key global-map [menu-bar tools]) nil
                    ["World Clock"
                     world-clock
                     :help "Display a world clock buffer with times in \
various time zones."]
                    "Calendar")

(easy-menu-add-item (lookup-key global-map [menu-bar tools]) nil
                    ["--"
                     separator-re]
                    "Calendar")

(provide 'cc-menu-reconfig)

;;; cc-menu-reconfig.el ends here
