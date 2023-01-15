(defun datestamp ()
  (interactive)
  (insert (format-time-string "*** %a %h %d %H:%M:%S %Z %Y")))

(defun datestamp2 ()
  "Datestamp string using format used by 'date' utility."
  (format-time-string "*** %a %h %d %H:%M:%S %Z %Y"))

;; Scrolling
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun shell-new()
  "Same as \\[shell], but starts new shell whether or not there
already exists one, giving it a unique name.
The current directory will be used.
A new frame will be created if pop-up-frames is t"
  (interactive)
  (let ((new-shell-name (generate-new-buffer-name "*shell*"))
        (curr-buf (current-buffer)))
    (if (string= new-shell-name "*shell*")
      (shell)
      (let ((tmp-buf-name (generate-new-buffer-name "tmpshell")))
        (switch-to-buffer "*shell*")
        (rename-buffer tmp-buf-name)
        (switch-to-buffer curr-buf)
        (shell)
        (rename-buffer new-shell-name)
        (switch-to-buffer tmp-buf-name)
        (rename-buffer "*shell*")))
    (switch-to-buffer curr-buf)
    (if pop-up-frames
      (switch-to-buffer-other-frame new-shell-name)
      (switch-to-buffer new-shell-name))))

(defvar journalFile "~/Dropbox/Documents/journal/journal.txt")
(defun journal()
  (interactive)
  (shell-command "echo '**' `date`  >> $HOME/Dropbox/Documents/journal/journal.txt")
  (find-file journalFile)
  (end-of-buffer)
  )

(defvar notesFile "~/Documents/journal/notes.txt")
(defun notes()
  (interactive)
  (shell-command "echo '* ' >> $HOME/Documents/journal/notes.txt")
  (find-file notesFile)
  (hide-body)
  (end-of-buffer)
  (backward-char)
  )

(defun tn()
  (interactive)
  (dired "~/Projects/Tile/Documents/Notes"))

(defun status-report()
  (interactive)
  (find-file (format-time-string "~/org/%Y_%m_%d.org"))
  (end-of-buffer))

(defun dictate()
   "dictate"
   (interactive)
   (shell-command "open ~/Documents/Dictation.txt"))


(fset 'gh-pr-link
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("[1;5DOD[OC](https://github.com/TileCorporation/tileapp_ios./pull/)" 0 "%d")) arg)))


(load-file (concat user-emacs-directory "url-bookmarks.el"))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defun cc/open-url (key)
  (interactive (list (completing-read-default "Open URL: " (alist-keys cc/url-bookmarks))))
  (browse-url (cdr (assoc key cc/url-bookmarks))))

(defun year ()
  (interactive)
  (shell-command (format-time-string "open ~/org/%Y.pdf")))

(defun make-year ()
  (interactive)
  (shell-command "cd ~/org; make year"))

(defun ia-writer-timestamp-to-org (beginning end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring beginning end)))
	(delete-region beginning end)
	(insert (format-time-string "<%Y-%m-%d %a %H:%M>" (encode-time (parse-time-string regionp)))))
    (message "The region is still there (from % d to %d), but it is inactive" 
             beginning end)))

(cl-defun chance (&key (win "You win.") &key (lose "You lose."))
  (interactive)
  (message (if (<= (* 100 (cl-random 1.0)) (read-number "Chance (%): ")) win lose))
  )

(fset 'ccstart
      (kmacro-lambda-form [f5 ?\C-c ?a ?a ?\C-x ?+ ?\C-x ?o] 0 "%d"))

(defun cc/org-time-stamp-inactive ()
  (interactive)
  (org-time-stamp-inactive '(16)))

(defun cc/select-journal-file ()
  (interactive)
  (find-file
   (concat "~/org/"
           (concat
            (replace-regexp-in-string "-" "_" (org-read-date))
            ".org"))))

;; This is a copy from s.el to enable early loading
(defun s-replace (old new s)
  "Replaces OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun pelican-timestamp ()
   (interactive)
   (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun cc/new-blog-post ()
  (interactive)
    (cd "~/Projects/devnull/content")
    (find-file (format-time-string "nfdn_%Y_%m_%d_%H%M%S.md"))
    (yas-insert-snippet)
    )

(defun cc/launch-pelican ()
  (interactive)
  (process-send-string (get-buffer-process "*pelican*") "make devserver\n")
  (sleep-for 3)
  (shell-command "open http://localhost:8000")
  )

(defun cc/blog ()
  (interactive)
  (cond ((get-buffer "*pelican*")
         (switch-to-buffer "*pelican*"))
        (t
         (shell-new)
         (rename-buffer "*pelican*")
         (process-send-string (get-buffer-process "*pelican*") "cd ~/Projects/pelican\n")
         (process-send-string (get-buffer-process "*pelican*") "source .venv/bin/activate\n")
         (process-send-string (get-buffer-process "*pelican*") "cd ~/Projects/devnull\n")
         ()
         (if (display-graphic-p)
             (cc/launch-pelican)
           )
         (cc/new-blog-post)
         ))
  )

(defun cc/slugify (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (save-excursion
          (delete-region start end)
          (insert
           (replace-regexp-in-string
            "[^a-z0-9-]" ""
            (replace-regexp-in-string
             "\s+" "-"
             (downcase regionp)
             )))))))

(defun cc/posix-timestamp-to-human (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (set 'inputTime (time-convert (string-to-number regionp)))
        (set 'inputBuf (number-to-string (string-to-number regionp)))
        (set 'rfcBuf (format-time-string "%a, %e %b %Y %H:%M:%S %z" inputTime))
        (set 'isoBuf (format-time-string "%Y-%m-%dT%H:%M:%S%z" inputTime))
        (with-output-to-temp-buffer "*timestamps*"
          (princ (concat "| POSIX | " inputBuf " |\n"))
          (princ (concat "| RFC 822 | " rfcBuf " |\n"))
          (princ (concat "| ISO 8601 | " isoBuf " |\n"))
          ))))

(defun cc/human-timestamp-to-posix (start end)
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring start end)))
        (set 'result (number-to-string (time-to-seconds (date-to-time regionp))))
        (kill-new result)
        (with-output-to-temp-buffer "*timestamps*"
          (princ result)))))

(defun dm/copy-as-rtf ()
  "Export region to RTF and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

(defun cc/ediff-revision (e)
  "Invoke ediff-revision with buffer-file-name."
  (interactive "e")
  (ediff-revision buffer-file-name))

(defun cc/org-emphasize-bold ()
  (interactive)
  (org-emphasize ?*))

(defun cc/org-emphasize-italic ()
  (interactive)
  (org-emphasize ?/))

(defun cc/org-emphasize-code ()
  (interactive)
  (org-emphasize ?~))

(defun cc/org-emphasize-underline ()
  (interactive)
  (org-emphasize ?_))

(defun cc/org-emphasize-verbatim ()
  (interactive)
  (org-emphasize ?=))

(defun cc/org-emphasize-strike-through ()
  (interactive)
  (org-emphasize ?+))

(defun cc/org-emphasize-reset ()
  ;; this won't work when org-hide-emphasis-markers is turned on.
  (interactive)
  (org-emphasize ?\s))

;; Transform Text
(defvar cc/transform-text-menu (make-sparse-keymap "Transform Text"))

(define-key cc/transform-text-menu [tranform-text-uppercase]
  '(menu-item "Make Upper Case" upcase-region
              :help "Upper case region"))

(define-key-after cc/transform-text-menu [tranform-text-lowercase]
  '(menu-item "Make Lower Case" downcase-region
              :help "Lower case region"))

(define-key-after cc/transform-text-menu [tranform-text-capitalize]
  '(menu-item "Capitalize" capitalize-region
              :help "Capitalize region"))

;; Org Emphasize
(defvar cc/org-emphasize-menu (make-sparse-keymap "Org Emphasize"))

(define-key cc/org-emphasize-menu [org-emphasize-bold]
  '(menu-item "Bold" cc/org-emphasize-bold
              :help "Bold"))

(define-key-after cc/org-emphasize-menu [org-emphasize-italic]
  '(menu-item "Italic" cc/org-emphasize-italic
              :help "Italic"))

(define-key-after cc/org-emphasize-menu [org-emphasize-code]
  '(menu-item "Code" cc/org-emphasize-code
              :help "Code"))

(define-key-after cc/org-emphasize-menu [org-emphasize-underline]
  '(menu-item "Underline" cc/org-emphasize-underline
              :help "Underline"))

(define-key-after cc/org-emphasize-menu [org-emphasize-verbatim]
  '(menu-item "Verbatim" cc/org-emphasize-verbatim
              :help "Verbatim"))

(define-key-after cc/org-emphasize-menu [org-emphasize-strike-through]
  '(menu-item "Strike Through" cc/org-emphasize-strike-through
              :help "Strike through"))

(define-key-after cc/org-emphasize-menu [org-emphasize-reset]
  '(menu-item "Reset" cc/org-emphasize-reset
              :help "Remove emphasis"))

;; Markdown Emphasize
(defvar cc/markdown-emphasize-menu (make-sparse-keymap "Markdown Emphasize"))

(define-key cc/markdown-emphasize-menu [markdown-emphasize-bold]
  '(menu-item "Bold" markdown-insert-bold
              :help "Bold"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-italic]
  '(menu-item "Italic" markdown-insert-italic
              :help "Italic"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-code]
  '(menu-item "Code" markdown-insert-code
              :help "Code"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-strike-through]
  '(menu-item "Strike Through" markdown-insert-strike-through
              :help "Strike through"))

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)
    (define-key-after menu [open-in-finder]
      '(menu-item "Open in Finder" reveal-in-folder-this-buffer
                  :help "Open file (buffer) in Finder"))

    (when (region-active-p)
      (define-key-after menu [osx-dictionary-lookup]
        '(menu-item "Look up" osx-dictionary-search-word-at-point
                    :help "Look up in dictionary"))

      (define-key-after menu [occur-word-at-mouse]
        '(menu-item "Occur" occur-word-at-mouse
                    :help "Occur")))
    
    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (define-key-after menu [vc-separator]
        '(menu-item "--single-line"))
      
      (define-key-after menu [magit-status]
        '(menu-item "Magit Status" magit-status
                    :help "Magit Status"))
      (define-key-after menu [ediff-revision]
        '(menu-item "Ediff revision…" cc/ediff-revision
                    :help "Ediff this file with revision")))
    
    (when (region-active-p)
      (define-key-after menu [transform-text-separator]
        '(menu-item "--single-line"))
      (define-key-after menu [tranform-text]
        (list 'menu-item "Transform" cc/transform-text-menu)))

    (when (and (derived-mode-p 'markdown-mode) (region-active-p))
      (define-key-after menu [markdown-emphasize]
        (list 'menu-item "Style" cc/markdown-emphasize-menu)))
    
    (when (and (derived-mode-p 'org-mode) (region-active-p))
      (define-key-after menu [org-emphasize]
        (list 'menu-item "Style" cc/org-emphasize-menu))

      (define-key-after menu [org-export-to-slack]
        '(menu-item "Copy as Slack" org-slack-export-to-clipboard-as-slack
                    :help "Copy as Slack to clipboard"))
      
      (define-key-after menu [copy-as-rtf]
        '(menu-item "Copy as RTF" dm/copy-as-rtf
                    :help "Copy as RTF to clipboard")))

    (when (org-at-table-p)
      (define-key-after menu [org-table-separator]
        '(menu-item "--single-line"))
      (define-key-after menu [org-table-field-info]
        '(menu-item (format "@%d$%d"
                            (org-table-current-dline)
                            (org-table-current-column))
                    cc/kill-org-table-reference
                    :help "Table field/cell information")))

    (when (region-active-p)
      (define-key-after menu [google-search]
        '(menu-item "Search with Google" google-this-noconfirm
                    :help "Search Google with region"))))
      
  menu)


(defun cc/kill-org-table-reference (e)
  (interactive "e")
  (kill-new (format "@%d$%d"
                    (org-table-current-dline)
                    (org-table-current-column))))

(add-hook 'context-menu-functions #'cc/context-menu-addons)

(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)


;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash."
  (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"port install trash\"")
  (call-process "trash" nil 0 nil "-F"  file))

(defvar my/re-builder-positions nil
  "Store point and region bounds before calling re-builder")
(advice-add 're-builder
            :before
            (defun my/re-builder-save-state (&rest _)
              "Save into `my/re-builder-positions' the point and region positions before calling `re-builder'."
              (setq my/re-builder-positions
                    (cons (point)
                          (when (region-active-p)
                            (list (region-beginning)
                                  (region-end)))))))


(defun reb-replace-regexp (&optional delimited)
  "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
  (interactive "P")
  (reb-update-regexp)
  (let* ((re (reb-target-binding reb-regexp))
         (replacement (query-replace-read-to
                       re
                       (concat "Query replace"
                               (if current-prefix-arg
                                   (if (eq current-prefix-arg '-) " backward" " word")
                                 "")
                               " regexp"
                               (if (with-selected-window reb-target-window
                                     (region-active-p)) " in region" ""))
                       t))
         (pnt (car my/re-builder-positions))
         (beg (cadr my/re-builder-positions))
         (end (caddr my/re-builder-positions)))
    (with-selected-window reb-target-window
      (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
      (setq my/re-builder-positions nil)
      (reb-quit)
      (query-replace-regexp re replacement delimited beg end))))


;(define-key reb-mode-map (kbd "RET") #'reb-replace-regexp)
;(define-key reb-lisp-mode-map (kbd "RET") #'reb-replace-regexp)

(defun arrayify (start end quote)
    "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

