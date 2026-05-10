;;; cclisp.el --- Utility Functions -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Charles Choi

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
(require 'window)
(require 'ediff)
(require 'map)
(require 'transient)
(require 'bookmark)
;; (require 'spotlight)
(require 'org-capture)
(require 'org-agenda)
(require 'org-table)
(require 'yasnippet)
(require 'org-ql-view)
(require 'calc)
(require 'project)
(require 'ace-window)
(require 'which-func)
(require 'casual-lib)
(require 'info)
(require 'transpose-frame)
(require 'dired)
(require 'page-ext)

(defun cc/find-user-init-file ()
  "Edit `user-init-file'."
  (interactive)
  (find-file user-init-file))

(defun datestamp ()
  "Insert datestamp intended for Charles Choi org notes."
  (interactive)
  (insert (format-time-string "** %a %h %d %H:%M:%S %Z %Y")))

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
  "Redirection for `status-report' for Charles Choi."
  (interactive)
  (cond
   ((string= (system-name) "bingsu.local")
    (status-report))

   ((string= (system-name) "dev7")
    (status-report-dev7))

   (t
    (status-report))))

(defun status-report()
  "Open the daily journal file for Charles Choi and go to the end of buffer."
  (interactive)
  (find-file (format-time-string "~/org/%Y_%m_%d.org"))
  (goto-char (point-max)))

(defun status-report-dev7 ()
  "Open journal file on dev7."
  (interactive)
  (find-file "~/Documents/journal/journal.org"))

(defun dictate()
   "Open a default text file to dictate into using macOS open."
   (interactive)
   (let ((fname (file-name-concat "~/org/dictation"
                                  (format-time-string "%Y%m%d_%H%M%S.txt"))))
     (shell-command (format "touch %s" fname))
     (shell-command (format "open -a TextEdit.app %s" fname))))

;; TODO: revisit storing web links
;; (load-file (concat user-emacs-directory "url-bookmarks.el"))

;; (defun cc/open-url ()
;;   "Open URL from file `cc/url-bookmarks'."
;;   (interactive)
;;   (let ((choice (car (completing-read-multiple "Select URL: " (map-keys cc/url-bookmarks)))))
;;     (browse-url (cdr (assoc choice cc/url-bookmarks)))))

(defun year (arg)
  "Open daily generated current year PDF file.
If prefix ARG is invoked, then macOS open is used to open the PDF file."
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

(defun cc/org-set-creation-timestamp ()
  "Set Org property “CREATED”."
  (interactive)
  (org-set-property "CREATED" (org-time-stamp-inactive '(16))))

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

(defun cc/posix-timestamp-to-human (start end)
  "Convert a POSIX timestamp bounded by START and END to RFC 822 and \
ISO 8601."
  (interactive "r")
  (if (use-region-p)
      (let* ((regionp (buffer-substring start end))
             (inputTime (time-convert (string-to-number regionp) 'list))
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
  "Export region to RTF and copy it to the clipboard.

Code taken from
URL `;https://gist.github.com/danielmartin/3c5d3a3a8cd24a3556379c5251651748'."
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


(defgroup kickingvegas nil
  "Settings for kickingvegas."
  :group 'convenience)

(defcustom cc-speech-rate-korean 80
  "Speech rate for say utility when speaking 한글."
  :type 'integer
  :group 'kickingvegas)

(defcustom cc-speech-voice-korean "Yuna"
  "Speech voice for say utility when speaking 한글."
  :type 'string
  :group 'kickingvegas)

(defun cc/say-region-korean (&optional start end)
  "Pass 한글 region bounded by START and END to macOS say command.

The voice and speech rate are configurable with the following variables:

- `cc-speech-voice-korean'
- `cc-speech-rate-korean'"
  (interactive "r")

  (let* ((cmdlist ())
         (cmdlist (push "say" cmdlist))
         (cmdlist (push "-v" cmdlist))
         (cmdlist (push (format "'%s'" cc-speech-voice-korean) cmdlist))
         (cmdlist (push "-r" cmdlist))
         (cmdlist (push (format "%d" cc-speech-rate-korean) cmdlist))
         ;; async generates a stupid window
         ;; (payload (buffer-substring-no-properties start end))
         ;; (cmdlist (push payload cmdlist))
         ;; (cmdlist (push "&" cmdlist))
         (cmdlist (reverse cmdlist))
         (cmd (string-join cmdlist " ")))
    ;; (shell-command cmd)
    (shell-command-on-region start end cmd)))

(defun cc/ellipsis()
  "Insert an ellipsis."
  (interactive)
  (insert "…"))

(defun cc/triangular-bullet ()
  "Insert a triangular bullet."
  (interactive)
  (insert "‣"))

(defun cc/menu-symbol ()
  "Insert a menu symbol."
  (interactive)
  (insert "›"))

(defun cc/prefix-symbol ()
  "Insert a prefix symbol."
  (interactive)
  (insert "✦"))

(defun cc/info-symbol ()
  "Insert an info symbol."
  (interactive)
  (insert "ⓘ"))

(defun cc/option-symbol ()
  "Insert an option symbol."
  (interactive)
  (insert "⌥"))

(defun cc/command-symbol ()
  "Insert an option symbol."
  (interactive)
  (insert "⌘"))

(defun cc/apple-maps-search(&optional input)
  "Search Apple Maps with INPUT.
\nIf a region is active this command will use it as INPUT, otherwise it
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
  "Open region from START to END in Apple Maps."
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
  "Phone call the selected number (region) bounded between START and END."
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
  "Duplicate the current file in Dired."
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

(defun cc/display-notification (msg &optional title subtitle sound)
  "Display macOS notification via osascript with MSG, TITLE, SUBTITLE, SOUND.
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
  "Transient supporting version of `bookmark-bmenu-list'."
  (interactive)
  (bookmark-maybe-load-default-file)
  (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
    (switch-to-buffer buf))
  (bookmark-bmenu-mode)
  (bookmark-bmenu--revert))

(defun cc/copy-word ()
  "Copy word after point."
  (interactive)
  (mark-word)
  (kill-ring-save (region-beginning) (region-end)))

(defun cc/copy-sentence ()
  "Copy sentence after point."
  (interactive)
  (save-excursion
    (set-mark (point))
    (forward-sentence)
    (kill-ring-save (region-beginning) (region-end))))

(defun cc/copy-paragraph ()
  "Copy paragraph point is in."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (kill-ring-save (region-beginning) (region-end))))

(defun cc/copy-defun ()
  "Copy defun point is in."
  (interactive)
  (save-excursion
    (mark-defun)
    (kill-ring-save (region-beginning) (region-end))))

(defun cc/copy-sexp ()
  "Copy sexp after point."
  (interactive)
  (mark-sexp)
  (kill-ring-save (region-beginning) (region-end)))

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

;; TODO: obsolete
;; (defun cc/dired-image-file-p ()
;;   "Predicate if current file in Dired is an image file."
;;   (string-match-p (image-dired--file-name-regexp) (dired-get-filename)))

;; TODO: obsolete
;; (defun cc/--image-info (filename)
;;   "Get image information of FILENAME via Imagemagick identify utility."
;;   (car
;;    (process-lines
;;     "identify"
;;     "-format"
;;     "%m %wx%h %b"
;;     (expand-file-name filename))))

(defun cc/ssh (target)
  "Create ssh `term' to TARGET."
  (interactive "suser@host: ")
  (term
   (concat "ssh " target))
  (switch-to-buffer "*terminal*")
  (rename-buffer (format "*ssh %s*" target)))

(defun cc/browse-backward-paragraph ()
  "Move point backward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (backward-paragraph 2)
  (forward-line))

(defun cc/browse-forward-paragraph ()
  "Move point forward paragraph such that the first line is highlighted.
\nThis function is intended to be used with `hl-line-mode'."
  (interactive)
  (forward-paragraph)
  (forward-line))

(defun cc/browse-forward-sexp ()
  "Move point forward sexp such that the first line is highlighted."
  (interactive)
  (forward-sexp 2)
  (backward-sexp))

(defun cc/browse-backward-sexp ()
  "Move point backward sexp such that the first line is highlighted."
  (interactive)
  (backward-sexp))

(defun cc/enable-local-sentence-double-space ()
  "Enable `sentence-end-double-space' locally."
  (interactive)
  (setq-local sentence-end-double-space t))

(defun weather (location)
  "Call weather script with LOCATION and show result in minibuffer."
  (interactive "sWhere (default: local): ")

  (let* ((weather-cmd "weather")
         (cmd (if location (format "%s %s" weather-cmd location) weather-cmd))
         (result (shell-command-to-string cmd)))
    (kill-new result)
    (message result)))

(defun melpa-package-status (package-name)
  "Get current timestamp of a MELPA PACKAGE-NAME.

Invokes python script ‘melpa-package-status.py’."
  (interactive "sPackage Name: ")
  (let* ((cmd (format "%s %s" "melpa-package-status" package-name))
         (result (shell-command-to-string cmd)))
    (kill-new result)
    (message result)))

(defun cc/describe-function-point-is-in ()
  "Describe enclosing Elisp function at point.
\nInvoke `describe-function' on the enclosing Elisp function the
point is in.

Thanks to mwnaylor, PropagandaOfTheDude, and deaddyfreddy for
helping write this function."
  (interactive)
  (when-let ((interned (intern-soft (which-function))))
    (describe-function interned)))

(defun cc/repunctuate-and-fill-paragraph ()
  "Fill paragraph with repunctuated sentences.

This command refills the paragraph surrounding the point such
that sentences are double space separated. For this function to
work properly, the point must be within a paragraph that has a
blank line before its start and after its end."
  (interactive)
  (backward-paragraph)
  (mark-paragraph)
  (repunctuate-sentences t)
  (deactivate-mark)
  (fill-paragraph))

(defun cc/casual-convert-menu-entry-to-test-vector ()
  "Convert Transient menu item into a casualt test vector.

If the menu item persists the transient (e.g. :transient t),
then you should put a ‘q’ at the end of the key macro string."
  (interactive)
  (back-to-indentation)
  (forward-char)
  (forward-sexp)
  (kill-sexp)
  (insert-char ?  )
  (insert-char ?.)
  (forward-sexp)
  (kill-sexp)
  (kill-sexp))

;; Transient Labels

;; TODO: obsolete
(defun cc/--variable-to-checkbox (v)
  "Checkbox string representation of variable V.
V is either nil or non-nil."
  (if (display-graphic-p)
      (if v "☑︎" "◻︎")
    (if v "[x]" "[ ]")))

;; TODO: obsolete
(defun cc/--prefix-label (label prefix)
  "Label constructed with PREFIX and LABEL separated by a space."
  (format "%s %s" prefix label))

;; TODO: obsolete
(defun cc/--checkbox-label (v label)
  "Checkbox label using variable V and LABEL."
  (cc/--prefix-label label (cc/--variable-to-checkbox v)))

(defun cc/smart-single-quote-region (start end)
  "Enclose region within START and END in smart single quotes."
  (interactive "r")
  (let* ((content (string-trim (buffer-substring start end))))
    (delete-region start end)
    (insert (concat "‘" content "’"))))

(defun cc/smart-double-quote-region (start end)
  "Enclose region within START and END in smart double quotes."
  (interactive "r")
  (let* ((content (string-trim (buffer-substring start end))))
    (delete-region start end)
    (insert (concat "“" content "”"))))

(defun cc/smart-single-quotes ()
  "Insert smart single quotes."
  (interactive)
  (insert "‘’"))

(defun cc/smart-double-quotes ()
  "Insert smart double quotes."
  (interactive)
  (insert "“”"))

(defun cc/apostrophe ()
  "Insert smart apostrophe."
  (interactive)
  (insert "’"))

(defun cc/show-fn-key-bindings ()
  "Show function key bindings."
  (interactive)
  (call-interactively #'describe-bindings)
  (switch-to-buffer "*Help*")
  (occur "^.*<f[[:digit:]]*>")
  (delete-other-windows))


(defun cc/find-test-file ()
  "Open test file in other window."
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (test-name (concat "../tests/test-" filename)))
    (find-file-other-window test-name)
    (transpose-frame)))


(defun cc/clear-mouse-overlay ()
  "Clear secondary overlay in buffer.

  Workaround fix for mouse rectangle selects."
  (interactive)
  (delete-overlay mouse-secondary-overlay))

(defun cc/toggle-unicode ()
  "Toggle Unicode and prettify symbols."
  (interactive)
  ;;(prettify-symbols-mode 'toggle)
  (if casual-lib-use-unicode
      (progn
        (setopt casual-lib-use-unicode nil)
        (prettify-symbols-mode -1))
    (progn
      (setopt casual-lib-use-unicode t)
      (prettify-symbols-mode nil))))

(defun macports ()
  "Run MacPorts."
  (interactive)
  (let* ((mbuffer (get-buffer "*macports*")))
    (if mbuffer
        (switch-to-buffer mbuffer)
      (progn
        (term "~/bin/port.sh")
        (rename-buffer "*macports*")))))

(defun swift-repl ()
  "Swift repl."
  (interactive)
  (term "swift repl")
  (rename-buffer "*swift*"))

;; TODO: obsolete
(defun cc/--next-sexp-raw ()
  "Raw implementation to move point to the beginning of the next sexp.

This function has no error checking."
  (forward-sexp 2)
  (backward-sexp))

;; TODO: obsolete
(defun cc/next-sexp ()
  "Move point to beginning of the next balanced expression (sexp)."
  (interactive)
  (condition-case nil
      (cc/--next-sexp-raw)
    (error (condition-case nil
               (forward-sexp)
             (error
              (message
               "Unable to move point to next balanced expression (sexp)."))))))

(defun cc/scratch-buffer ()
  "Edit scratch buffer as a side window."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (scratch-buffer))

(defun cc/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))

(defun cc/org-copy-region-as (backend)
  "Copy the BACKEND exported Org region to the system clipboard.

Code derived from Marcin Borkowski post at
URL `https://mbork.pl/2021-05-02_Org-mode_to_Markdown_via_the_clipboard'"
  (interactive)
  (if (use-region-p)
      (let* ((region
              (buffer-substring-no-properties
               (region-beginning)
               (region-end)))
             (clipping
              (org-export-string-as region backend t '(:with-toc nil))))
        (gui-set-selection 'CLIPBOARD clipping))))

(defun cc/org-copy-region-as-markdown ()
  "Copy the Markdown exported Org region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (cc/org-copy-region-as 'md)))

(defun cc/org-copy-region-as-gfm ()
  "Copy the GitHub Markdown exported Org region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (cc/org-copy-region-as 'gfm)))

(defun cc/org-copy-region-as-latex ()
  "Copy the LaTeX exported Org region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (cc/org-copy-region-as 'latex)))

(defun cc/org-copy-region-as-ascii ()
  "Copy the ASCII exported Org region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (cc/org-copy-region-as 'ascii)))

(defun cc/org-copy-region-as-html ()
  "Copy the HTML exported Org region to the system clipboard."
  (interactive)
  (if (use-region-p)
      (cc/org-copy-region-as 'html)))

(defun cc/yank-markdown-as-org ()
  "Yank Markdown text as Org.

This command will convert Markdown text in the top of the `kill-ring'
and convert it to Org using the pandoc utility."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (yank)
      (shell-command-on-region
       (point-min) (point-max)
       "pandoc -f markdown -t org --wrap=preserve" t t)
      (kill-region (point-min) (point-max)))
    (yank)))


(defun cc/split-window-right ()
  "Invoke `split-window-right', making the new window active."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun cc/split-window-below ()
  "Invoke `split-window-below', making the new window active."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun cc/dictation-reset ()
  "Reset macOS dictation service corespeechd."
  (interactive)
  (process-lines "killall" "corespeechd"))

(defun cc/info-compile ()
  "Build Info file from an Org file."
  (interactive)
  (let ((outfile (expand-file-name (file-name-with-extension buffer-file-name "info")))
        ;; (texi (file-name-with-extension buffer-file-name "texi"))
        )

    (org-texinfo-export-to-info)
    ;;(org-export-to-file 'texinfo texi)
    ;;(process-lines "make" "run")
    (if (get-buffer "*info*")
        (kill-buffer "*info*"))
    (info outfile)
    (info-initialize)))

(defun cc/casual-info-compile ()
  "Build Casual Info file."
  (interactive)
  (let* ((outfile "~/Projects/elisp/casual/docs/casual.info")
         (current (current-buffer)))
    (find-file "~/Projects/elisp/casual/docs/casual.org")
    (org-texinfo-export-to-info)
    (if (get-buffer "*info*")
        (kill-buffer "*info*"))
    (info outfile)
    (info-initialize)
    (switch-to-buffer current)))

(defun cc/anju-info-compile ()
  "Build Anju Info file."
  (interactive)
  (let* ((outfile "~/Projects/elisp/anju/docs/anju.info")
         (current (current-buffer)))
    (find-file "~/Projects/elisp/anju/docs/anju.org")
    (org-texinfo-export-to-info)
    (if (get-buffer "*info*")
        (kill-buffer "*info*"))
    (info outfile)
    (info-initialize)
    (switch-to-buffer current)))

(defun cc/load-casual-info ()
  "Load Casual info file."
  (interactive)
  (info "~/Projects/elisp/casual/docs/casual.info")
  (info-initialize))

(defun cc/show-global-map-keys (keypath)
  "Show formatted keys for keymap in `global-map' given KEYPATH."
  (interactive)
  (mapcar (lambda (x) (format "%s" x))
          (mapcar (lambda (x) (if (listp x) (car x)))
                  (cdr (lookup-key global-map keypath)))))

(defun cc/whitespace-cleanup (&optional disable)
  "Turn on whitespace cleanup with optional DISABLE."
  (interactive)

  (if disable
      (progn
        (setopt show-trailing-whitespace nil)
        (remove-hook 'before-save-hook #'whitespace-cleanup))

    (progn
      (setopt show-trailing-whitespace t)
      (add-hook 'before-save-hook #'whitespace-cleanup))))

(defun cc/toggle-scrolling-config ()
  "Tune scrolling behavior to support mouse or text based scrolling."
  (interactive)

  (if (equal scroll-conservatively 10)
      (progn
        (setopt scroll-conservatively 0)
        (setopt scroll-margin 0)
        (message "Reset to default"))
    (progn
        (setopt scroll-conservatively 10)
        (setopt scroll-margin 15)
        (message "Optimized for text scrolling"))))

(defun cc/--resize-frame (width height)
  "Resize frame to WIDTH, HEIGHT."
  (let* ((current (selected-frame)))
    (set-frame-size current width height)))

(defun cc/frame-resize-for-video ()
  "Resize frame for 1024x768 video capture."
  (interactive)
  (cc/--resize-frame 108 39))

(defun cc/frame-resize-for-tty ()
  "Resize frame for terminal screenshot."
  (interactive)
  (cc/--resize-frame 86 28))

(defun cc/frame-resize-for-desktop ()
  "Resize frame for desktop usage."
  (interactive)
  (cc/--resize-frame 157 88)
  (set-frame-position (selected-frame) 780 39))

(defun cc/frame-resize (arg)
  "Resize frame to prompted value, moving frame if prefix ARG is non-nil.

This command is tuned for macOS using a single display."
  (interactive "P")
  (let* ((choice
          (completing-read "Display Configuration: "
                           '("desktop" "macbook" "tty" "standard" "focus" "video")
                           nil nil "standard"))
         (move (not arg)))
    (cond
     ((string-equal choice "desktop")
      (cc/--resize-frame 157 88)
      (if move
          (set-frame-position (selected-frame) 780 39)))

     ((string-equal choice "macbook")
      (cc/--resize-frame 164 49)
      (if move
          (set-frame-position (selected-frame) 0 38)))

     ((string-equal choice "video")
      (cc/--resize-frame 108 39))

     ((string-equal choice "tty")
      (cc/--resize-frame 86 28))

     ((string-equal choice "focus")
      (cc/--resize-frame 87 35)
      (if move
          (set-frame-position (selected-frame) 1095 534)))

     ((string-equal choice "standard")
      (cc/--resize-frame 141 71)
      (if move
          (set-frame-position (selected-frame) 852 192)))

     (t
      (error "Unknown display size")))))

(defun cc/--dired-kill-image-buffer-before-delete (file &rest rest)
  "Kill buffer associated with image FILE if necessary, ignoring REST."
  (ignore rest)
  (let* ((test-types (push 'jpg image-types))
         (ext (file-name-extension file))
         (buf (get-file-buffer file)))

    (if (and buf (seq-contains-p test-types ext #'string-equal))
        (progn
          (message "Killed buffer %s" (buffer-name buf))
          (kill-buffer buf)))))

(advice-add 'dired-delete-file
            :before #'cc/--dired-kill-image-buffer-before-delete)

(defun cc/org-gen-custom-id ()
  "Generate a UUID, insert it as an Org :CUSTOM_ID: property, and return link."
  (interactive)
  (let* ((custom-id (format "%s" (org-id-uuid)))
         (components (org-heading-components))
         (header (nth 4 components))
         (org-link (format "[[#%s][%s]]" custom-id header)))

    (save-excursion
      (org-back-to-heading t)
      (if (re-search-forward "^:CUSTOM_ID:" (save-excursion (outline-next-heading) (point)) t)
          (progn
            (beginning-of-line)
            (kill-line)
            (insert (format ":CUSTOM_ID: %s\n" custom-id)))
        (org-set-property "CUSTOM_ID" custom-id)))
    (kill-new org-link)
    ;;(org-insert-link nil (format "#%s" custom-id) header)
    (message "Copied %s" org-link)
    org-link))

(defun cc/backward-page-at-top (&optional count)
  "Move backward COUNT pages, scrolling point to top of window."
  (interactive)

  (if (buffer-narrowed-p)
      (pages-previous-page count)
    (progn
      (backward-page count)
      (recenter-top-bottom 0))))

(defun cc/forward-page-at-top (&optional count)
  "Move forward COUNT pages, scrolling point to top of window."
  (interactive)
  (if (buffer-narrowed-p)
      (pages-next-page count)
    (progn
      (forward-page count)
      (recenter-top-bottom 0))))

;; TODO: rename to cc/page-break
(defun cc/line-feed ()
  "Insert line feed."
  (interactive)
  (insert "\n")
  (if (derived-mode-p 'emacs-lisp-mode)
      (insert ";; -------------------------------------------------------------------\n")))


(defun cc/ert-test-gen ()
  "Generate ERT test for define and put into the `kill-ring'."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let* ((fn (list-at-point))
           (fn-name (symbol-name (seq-elt fn 1)))
           (docstr (format "Test for `%s'." fn-name))
           (fn-ert ())
           (fn-ert (push docstr fn-ert))
           (fn-ert (push nil fn-ert))
           (fn-ert (push (intern (format "test-%s" fn-name)) fn-ert))
           (fn-ert (push 'ert-deftest fn-ert))
           (fn-ert-test (prin1-to-string fn-ert))
           (fn-ert-test (string-replace " nil " " ()\n  " fn-ert-test)))
      (kill-new fn-ert-test))))

(defun music ()
  "Launch Music app."
  (interactive)

  (cond
   ((or (eq window-system 'ns) (eq window-system 'mac))
    (process-lines "open" "-a" "Music.app"))
   (t
    (message "Unsupported"))))

(defun cc/cleanup-prog ()
  "Cleanup code buffer."
    (indent-region (point-min) (point-max))
    (whitespace-cleanup)
    (save-buffer))


(defun cc/--function-tool-tip (fn)
  "Generate tool tip from function FN."
  (let ((docstring (documentation fn)))
    (if docstring
        (replace-regexp-in-string "\.$" ""
                                  (car (string-split docstring "\n")))
      (error "No docstring in %s" fn))))

(defun cc/tool-tip-extract ()
  "Extract tool tip for symbol at point and put into `kill-ring'."
  (interactive)
  (kill-new (cc/--function-tool-tip (symbol-at-point))))

(defun cc/--defun-name ()
  "Name of defun at point."
  (interactive)
  (let ((start 0)
        (end 0))
    (save-excursion
      (beginning-of-defun)
      (down-list)
      (forward-sexp 2)
      (setq end (point))
      (backward-sexp)
      (setq start (point))
      (buffer-substring-no-properties start end))))


(defun cc/ert-run-test-at-point ()
  "Run the ERT test at point."
  (interactive)
  (let ((test-name (cc/--defun-name)))
        ;; (message "ERT: %s" test-name)
        (ert test-name)))

(provide 'cclisp)
;;; cclisp.el ends here
