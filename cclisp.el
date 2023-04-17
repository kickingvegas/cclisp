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

(defun journal()
  (interactive)
  (status-report))

(defun status-report()
  (interactive)
  (find-file (format-time-string "~/org/%Y_%m_%d.org"))
  (end-of-buffer))

(defun dictate()
   "dictate"
   (interactive)
   (shell-command "open ~/Documents/Dictation.txt"))

(load-file (concat user-emacs-directory "url-bookmarks.el"))

(defun alist-keys (alist)
  (mapcar 'car alist))

(defun cc/open-url (key)
  (interactive (list (completing-read-default "Open URL: "
                                              (alist-keys cc/url-bookmarks))))
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

(fset 'cc/start
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

(defun cc/pelican-timestamp ()
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

(defun cc/web-captee()
  (interactive)
  (cond ((get-buffer "*pelican*")
         (switch-to-buffer "*pelican*"))
        (t
         (shell-new)
         (rename-buffer "*pelican*")
         (process-send-string (get-buffer-process "*pelican*") "cd ~/Projects/pelican\n")
         (process-send-string (get-buffer-process "*pelican*") "source .venv/bin/activate\n")
         (process-send-string (get-buffer-process "*pelican*") "cd ~/Projects/pelican/captee\n")
         ()
         (if (display-graphic-p)
             (cc/launch-pelican)
           )
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


(load "cc-context-menu")

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


(defun cc/say-region (&optional b e)
  (interactive "r")
  (shell-command-on-region b e "say"))


(defun cc/dark-mode ()
    (interactive)
    (set-face-attribute 'minibuffer-prompt nil
                        :foreground "orange")
    
    (set-face-attribute 'org-table nil
                        :foreground "#00ff22")

    (set-face-attribute 'org-scheduled-previously nil
                        :foreground "light sky blue")

    (set-face-attribute 'Man-overstrike nil
                        :foreground "white")

    (set-face-attribute 'Man-underline nil
                        :foreground "chartreuse")

    (set-face-attribute 'org-document-title nil
                        :foreground "chartreuse")

    (set-face-attribute 'org-document-info nil
                        :foreground "chartreuse")
        
    (set-face-attribute 'org-hide nil
                        :foreground "black"))


(defun cc/light-mode ()
    (interactive)
    (set-face-attribute 'minibuffer-prompt nil
                        :foreground "dark magenta")
    
    (set-face-attribute 'org-table nil
                        :foreground "Blue1")

    (set-face-attribute 'org-scheduled-previously nil
                        :foreground "#2255ff")

    (set-face-attribute 'Man-overstrike nil
                        :foreground "dark slate blue")

    (set-face-attribute 'Man-underline nil
                        :foreground "MediumBlue")

    (set-face-attribute 'org-document-title nil
                        :foreground "midnight blue")

    (set-face-attribute 'org-document-info nil
                        :foreground "midnight blue")
    
    (set-face-attribute 'org-hide nil
                        :foreground "white"))


