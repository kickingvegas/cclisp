;;; cc-wgrep-mode.el --- wgrep configuration

;;; Commentary:
;;

(require 'wgrep)

(easy-menu-define cc/wgrep-menu nil
  "Keymap for wgrep menu"
  '("Writeable Grep"
    :visible (derived-mode-p 'grep-mode)
    ["Change to wgrep mode" wgrep-change-to-wgrep-mode
     :enable (equal buffer-read-only t)
     :help "Change to wgrep mode."]

    ["Finish Edit" wgrep-finish-edit
     :enable (not buffer-read-only)
     :help "Apply the changes to file buffers and exit."]

    ["Mark Current Line for Deletion" wgrep-mark-deletion
     :enable (not buffer-read-only)     
     :help "Mark as delete to current line (including newline)."]
        
    ["Toggle Readonly" wgrep-toggle-readonly-area
     :enable (not buffer-read-only)     
     :help "Toggle read-only area to remove a whole line."]
    
    ["Remove Change" wgrep-remove-change
     :enable (not buffer-read-only)     
     :help "Remove changes in the region between BEG and END."]
    
    ["Remove All Changes" wgrep-remove-all-change
     :enable (not buffer-read-only)     
     :help "Remove changes in the whole buffer."]
    
    ["Abort Changes and Exit" wgrep-abort-changes
     :enable (not buffer-read-only)
     :help "Discard all changes and return to original mode."]

    ["Exit" wgrep-exit
     :enable (not buffer-read-only)
     :help "Return to original mode."]))

;; (define-key map "\C-c\C-c" 'wgrep-finish-edit)
;; (define-key map "\C-c\C-d" 'wgrep-mark-deletion)
;; (define-key map "\C-c\C-e" 'wgrep-finish-edit)
;; (define-key map "\C-c\C-p" 'wgrep-toggle-readonly-area)
;; (define-key map "\C-c\C-r" 'wgrep-remove-change)
;; (define-key map "\C-x\C-s" 'wgrep-finish-edit)
;; (define-key map "\C-c\C-u" 'wgrep-remove-all-change)
;; (define-key map "\C-c\C-k" 'wgrep-abort-changes)
;; (define-key map "\C-x\C-q" 'wgrep-exit)


(provide 'cc-wgrep-mode)

;;; cc-wgrep-mode.el ends here
