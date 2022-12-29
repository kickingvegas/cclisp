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
  (shell-command "open ~/org/2022.pdf"))

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

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)
    (define-key-after menu [open-in-finder]
      '(menu-item "Open in Finder…" reveal-in-folder-this-buffer
                  :help "Open file (buffer) in Finder"))
    
    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (define-key-after menu [ediff-revision]
        '(menu-item "Ediff revision…" cc/ediff-revision
                    :help "Ediff this file with revision"))))
  menu)

(add-hook 'context-menu-functions #'cc/context-menu-addons)

(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)
