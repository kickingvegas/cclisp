;;; cclisp.el --- Utility Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>

;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Utility functions by Charles Choi

;;; Code:
(require 'ediff)
(require 'map)
(require 'transient)
(require 'bookmark)
(require 'spotlight)
(require 'org-capture)
(require 'yasnippet)

(defun datestamp ()
  "Insert datestamp intended for Charles Choi org notes."
  (interactive)
  (insert (format-time-string "*** %a %h %d %H:%M:%S %Z %Y")))

(defun datestamp2 ()
  "Datestamp string using format used by date utility."
  (format-time-string "*** %a %h %d %H:%M:%S %Z %Y"))

;; Scrolling
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or ARG lines)."
  (interactive "p")
  (scroll-up (or arg 1)))

(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or ARG lines)."
  (interactive "p")
  (scroll-down (or arg 1)))

(defun shell-new()
  "Create a new instance of `shell' but with a unique name.
The current directory will be used.
A new frame will be created if `pop-up-frames' is t."
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
  "Alias to invoke `status-report' for Charles Choi."
  (interactive)
  (status-report))

(defun status-report()
  "Open the daily journal file for Charles Choi and go to the end of buffer."
  (interactive)
  (find-file (format-time-string "~/org/%Y_%m_%d.org"))
  (goto-char (point-max)))

(defun dictate()
   "Open a default text file to dictate into using macOS open."
   (interactive)
   (shell-command "open ~/Documents/Dictation.txt"))

(load-file (concat user-emacs-directory "url-bookmarks.el"))

(defun cc/open-url ()
  (interactive)
  (let ((choice (car (completing-read-multiple "Select URL: " (map-keys cc/url-bookmarks)))))
    (browse-url (cdr (assoc choice cc/url-bookmarks)))))

(defun year (arg)
  "Open daily generated current year PDF file.
If prefix is invoked, then macOS open is used to open the PDF file."
  (interactive "P")
  (ignore arg)
  (if current-prefix-arg
      (shell-command (format-time-string "open ~/org/%Y.pdf"))
    (find-file-other-window (format-time-string "~/org/%Y.pdf"))))

(defun make-year ()
  "Invoke makefile target to generate daily current year pdf file."
  (interactive)
  (shell-command "cd ~/org; make year"))

(defun ia-writer-timestamp-to-org (beginning end)
  "Convert iA Writer timestamp in region demarked by BEGINNING and END to Org."
  (interactive "r")
  (if (use-region-p)
      (let ((regionp (buffer-substring beginning end)))
        (delete-region beginning end)
        (insert (format-time-string "<%Y-%m-%d %a %H:%M>" (encode-time (parse-time-string regionp)))))
    (message "The region is still there (from % d to %d), but it is inactive"
             beginning end)))

(cl-defun chance (&key (win "You win.") &key (lose "You lose."))
  "Tell me my chances from 0 to 100 with either the WIN or LOSE string."
  (interactive)
  (message (if (<= (* 100 (cl-random 1.0)) (read-number "Chance (%): ")) win lose))
  )

(fset 'cc/start
      (kmacro [f5 ?\C-c ?a ?a ?\C-x ?+ ?\C-x ?o] 0 "%d"))

(defun cc/org-time-stamp-inactive ()
  "Insert an inactive Org timestamp."
  (interactive)
  (org-time-stamp-inactive '(16)))

(defun cc/select-journal-file ()
  "Select one of Charles Choi's journal files to open in a buffer."
  (interactive)
  (find-file
   (concat "~/org/"
           (concat
            (replace-regexp-in-string "-" "_" (org-read-date))
            ".org"))))

;; This is a copy from s.el to enable early loading
(defun s-replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun cc/pelican-timestamp ()
  "Insert a timestamp recognized by the Pelican static site generator."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(defun cc/new-blog-post ()
  "Create a new blog post in a buffer for “notes from /dev/null”."
  (interactive)
    (cd "~/Projects/devnull/content")
    (find-file (format-time-string "nfdn_%Y_%m_%d_%H%M%S.md"))
    (yas-insert-snippet))

(defun cc/launch-pelican ()
  "Launch a local instance of the Pelican static site server.
This function presumes that the buffer *pelican* is in the correct directory."
  (interactive)
  (process-send-string (get-buffer-process "*pelican*") "make devserver\n")
  (sleep-for 3)
  (shell-command "open http://localhost:8000")
  )

(defun cc/blog ()
  "One-step operation to develop a blog post for “notes from /dev/null”."
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
           ))))

(defun cc/web-captee()
  "One-step operation to startup a devserver for the Captee website."
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
             (cc/launch-pelican)))))

(defun cc/slugify (start end)
  "Slugify the region bounded by START and END."
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
             (downcase regionp))))))))

(defun cc/posix-timestamp-to-human (start end)
  "Convert a POSIX timestamp bounded by START and END to RFC 822 and \
ISO 8601."
  (interactive "r")
  (if (use-region-p)
      (let* ((regionp (buffer-substring start end))
             (inputTime (time-convert (string-to-number regionp) "list"))
             (inputBuf (number-to-string (string-to-number regionp)))
             (rfcBuf (format-time-string "%a, %e %b %Y %H:%M:%S %z" inputTime))
             (isoBuf (format-time-string "%Y-%m-%dT%H:%M:%S%z" inputTime)))
        (with-output-to-temp-buffer "*timestamps*"
          (princ (concat "| POSIX | " inputBuf " |\n"))
          (princ (concat "| RFC 822 | " rfcBuf " |\n"))
          (princ (concat "| ISO 8601 | " isoBuf " |\n"))))))

(defun cc/human-timestamp-to-posix (start end)
  "Convert a human timestamp bounded by START and END to POSIX."
  (interactive "r")
  (if (use-region-p)
      (let* ((regionp (buffer-substring start end))
             (result (number-to-string (time-to-seconds (date-to-time regionp)))))
        (kill-new result)
        (with-output-to-temp-buffer "*timestamps*"
          (princ result)))))

(defun dm/copy-as-rtf ()
  "Export region to RTF and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (ignore html)
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash."
  (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"https://github.com/sindresorhus/macos-trash\"")
  (call-process "trash" nil 0 nil file))

(defun arrayify (start end quote)
  "Turn multi-line region bounded by START and END to one line delimited by QUOTE."
    (interactive "r\nMQuote: ")
    (let ((insertion
           (mapconcat
            (lambda (x) (format "%s%s%s" quote x quote))
            (split-string (buffer-substring start end)) ", ")))
      (delete-region start end)
      (insert insertion)))

(defun cc/say-region (&optional start end)
  "Pass region bounded by START and END to macOS say command."
  (interactive "r")
  (shell-command-on-region start end "say"))

(defun cc/switch-to-scratch ()
  "Switch to *scratch* buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun cc/ellipsis()
  "Insert an ellipsis."
  (interactive)
  (insert "…"))

(defun cc/triangular-bullet ()
  "Insert a triangular bullet."
  (interactive)
  (insert "‣"))

(defun cc/apple-maps-search(&optional input)
  "Search Apple Maps with INPUT.\n
If a region is active this command will use it as INPUT, otherwise it
will use the word at point."
  (interactive (list
                (read-string (format "Map Search (%s): "
                                     (if (region-active-p)
                                         (buffer-substring (region-beginning) (region-end))
                                       (thing-at-point 'word 'no-properties)))
                                     nil nil
                                     (if (region-active-p)
                                         (buffer-substring (region-beginning) (region-end))
                                       (thing-at-point 'word 'no-properties)))))

  (let* ((mapURL (concat "maps://?q=" (url-encode-url input))))
    (message "Searching for %s" input)
    (browse-url mapURL)))

(defun cc/open-region-in-apple-maps (&optional start end)
  "Open region from START to END in Apple Maps"
  (interactive "r")
  (let* ((query-buf (buffer-substring start end))
         (mapURL (concat "maps://?q=" (url-encode-url query-buf))))
    (message "Searching for %s" query-buf)
    (browse-url mapURL)))

(defvar cc/pat-nanp-international "^+1 \
[(]*\\([0-9]\\{3\\}\\)[)]*\
[\\. -]\\([0-9]\\{3\\}\\)[\\. -]\\([0-9]\\{4\\}\\)$"
  "Regexp for North American Numbering Plan phone number including +1.")

(defvar cc/pat-nanp "^[(]*\\([0-9]\\{3\\}\\)[)]*[\\. -]\
\\([0-9]\\{3\\}\\)[\\. -]\\([0-9]\\{4\\}\\)$"
  "Regexp for North American Numbering Plan phone number without +1.")

(defun cc/nanp-phone-number-to-url (phone)
  "Convert PHONE number string to url \"tel:\"."
  (cond
   ((string-match cc/pat-nanp-international phone)
    (replace-regexp-in-string cc/pat-nanp-international
                              "tel:+1-\\1-\\2-\\3" phone))
   ((string-match cc/pat-nanp phone)
    (replace-regexp-in-string cc/pat-nanp "tel:+1-\\1-\\2-\\3" phone))))

(defun cc/call-nanp-phone-number (&optional start end)
  "Phone call the selected number (region) bounded between START and END"
  (interactive "r")
  (let ((phone-buf (buffer-substring start end)))
    (browse-url (cc/nanp-phone-number-to-url phone-buf))))

(defun cc/nanp-phone-number-p ()
  "Predicate for PHONE number."
  (let ((phone (buffer-substring (region-beginning) (region-end))))
    (cond
     ((string-match cc/pat-nanp-international phone)
      t)
     ((string-match cc/pat-nanp phone)
      t)
     (t
      nil))))

(defun cc/dired-duplicate-file ()
  "Duplicate the current file in Dired"
  (interactive)
  (when (derived-mode-p 'dired-mode)
    (let* ((filename (dired-get-filename))
           (target (concat (file-name-sans-extension filename)
                           " copy"
                           (file-name-extension filename t))))
      (message target)
      (if (file-directory-p filename)
          (copy-directory filename target)
        (copy-file filename target)))))


(defun cc/move-word-backward ()
  "Move word to the right of point backward one word.
Point must be at the beginning of word."
  (interactive)
  (transpose-words 1)
  (forward-word -2))

(defun cc/move-word-forward ()
  "Move word to the right of point forward one word.
Point must be at the beginning of word."
  (interactive)
  (forward-word 1)
  (transpose-words 1)
  (forward-word -1))

(defun cc/move-sentence-backward ()
  "Move sentence to the right of point backward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (transpose-sentences 1)
  (forward-sentence -2))

(defun cc/move-sentence-forward ()
  "Move sentence to the right of point forward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (forward-sentence 1)
  (transpose-sentences 1)
  (forward-sentence -1))

(defun cc/move-sexp-backward ()
  "Move balanced expression (sexp) to the right of point backward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (transpose-sexps 1)
  (forward-sexp -2))

(defun cc/move-sexp-forward ()
  "Move balanced expression (sexp) to the right of point forward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (forward-sexp 1)
  (transpose-sexps 1)
  (forward-sexp -1))

(defun cc/display-notification (msg &optional title subtitle sound)
  "Display macOS notification via osascript with MSG, TITLE, SUBTITLE, SOUND
MSG - notification message
TITLE - notification title (optional)
SUBTITLE - notification subtitle (optional)
SOUND - sound file (optional)"

  (message msg)
  (let ((cmd "display notification"))
    (setq cmd (concat cmd " \"" msg "\""))

    (if sound
        (setq cmd (concat cmd " sound name \"" sound "\""))
      (setq cmd (concat cmd " sound name \"" "Blow.aiff" "\"")))
    (if title
        (setq cmd (concat cmd " with title \"" title "\"")))
    (if subtitle
        (setq cmd (concat cmd " subtitle \"" subtitle "\"")))

    (setq cmd (concat "osascript -e '" cmd "'"))
    ;; (message cmd)
    (shell-command cmd)))

(defun cc/org-search ()
  "Search Org notes in ~/org with REGEXP with rgrep."
  (interactive)
  (let ((query (read-string "Search Org Notes (regexp): ")))
    (grep-compute-defaults)
    (rgrep query "*.org" "~/org/" nil)
    (switch-to-buffer-other-window "*grep*")))

(defun cc/list-bookmarks-transient ()
  "Transient supporting version of `bookmark-bmenu-list'"
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (switch-to-buffer buf))
  (bookmark-bmenu-mode)
  (bookmark-bmenu--revert))

(transient-define-prefix cc/meta-search ()
  "Meta Search Menu"
  [["Open"
     ("f"
      "Find File"
      find-file
      :transient nil)
     ("j"
      "Bookmark Jump"
      bookmark-jump
      :transient nil)
     ("c"
      "Org Capture"
      org-capture
      :transient nil)
     ("e"
      "Mark sexp"
      mark-sexp
      :transient nil)]
   ["Search"
    ("F"
     "Fuzzy Find"
     helm-find-files
     :transient nil)
    ("r"
     "Find in Files (rgrep)"
     rgrep
     :transient nil)
    ("s"
     "Spotlight"
     spotlight-fast
     :transient nil)
    ("o"
     "Org Files"
     cc/org-search
     :transient nil)
    ("g"
     "Google"
     google-this-search
     :if display-graphic-p
     :transient nil)
    ("m"
     "Apple Maps"
     cc/apple-maps-search
     :if display-graphic-p
     :transient nil)]
   ["List"
    ("b"
     "Bookmarks"
     cc/list-bookmarks-transient
     :transient nil)
    ("u"
     "URLs"
     cc/open-url
     :if display-graphic-p
     :transient nil)
    ("R"
     "Recent Files"
     recentf-open-files
     :transient nil)
    ("J"
     "Journal Files"
     cc/select-journal-file
     :transient nil)]])

(defun cc/html-quote-entities-to-utf8 ()
  "Convert HTML quote entities to UTF8 in buffer."
  (interactive)
  (let ((quote-dict '(("&lsquo;" . "‘")
                      ("&rsquo;" . "’")
                      ("&ldquo;" . "“")
                      ("&rdquo;" . "”")
                      ("&apos;" . "'")
                      ("&quot;" . "\""))))
    (save-excursion
      (dolist (e quote-dict)
        (goto-char (point-min))
        (while (search-forward (car e) nil t)
          (replace-match (cdr e) nil t))))))

(defun cc/utf8-to-html-quote-entities ()
  "Convert UTF8 quote characters to HTML quote entities in buffer."
  (interactive)
  (let ((quote-dict '(("‘" . "&lsquo;")
                      ("’" . "&rsquo;")
                      ("“" . "&ldquo;")
                      ("”" . "&rdquo;")
                      ("'" . "&apos;")
                      ("\"" . "&quot;"))))
    (save-excursion
      (dolist (e quote-dict)
        (goto-char (point-min))
        (while (search-forward (car e) nil t)
          (replace-match (cdr e) nil t))))))

(defun cc/--image-info (filename)
  "Get image information via Imagemagick identify utility."
  (car
   (process-lines
    "identify"
    "-format"
    "%m %wx%h %b"
    (expand-file-name filename))))

(provide 'cclisp)
;;; cclisp.el ends here
