;;; cc-gh.el --- gh commands                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Choi

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

;;

;;; Code:
(require 'seq)
(require 'map)
(require 'vtable)
(require 'transient)

;; TODO: Rebind < and > to move point while staying in the same column.
;;
;; (length (vtable-objects (vtable-current-table)))
;; (line-number-at-pos)

;; TODO: Design bindings for table navigation. (n, p, P, N)
;; TODO: Explore :objects-function for regenerating table
;; TODO: Format column widths
;; TODO: Make Transient menu for Org and Markdown export

(defvar cc/gh--last-repo-history nil
  "Private variable to store last used GitHub repository name.")

(defvar cc/gh-repo-name nil
  "Local repository name.")

(defun cc/gh-read-repo (prompt)
  "Prompt the user with PROMPT, using the last history entry as the default input."
  (let* ((history 'cc/gh--last-repo-history)  ; Define the history variable
         (last-history-entry (car (symbol-value history))))  ; Get the last entry
    (string-trim (read-string prompt last-history-entry history))))


(defun cc/gh-md2org (buf)
  "Convert BUF text format from Markdown to Org."
  (save-excursion
    (with-temp-buffer
      (insert buf)
      (shell-command-on-region (point-min) (point-max)
                               "pandoc --to=org --wrap=preserve"
                               (current-buffer)
                               t)
      (buffer-string))))

(defun cc/gh-format-labels (labels)
  "Convert LABELS to a comma-separated string.

LABELS is a vector of hash-tables, each hash-table corresponding
to the JSON dictionary containing label information returned by
gh."
  (let ((temp-list (mapcar (lambda (label)
                             (map-elt label "name"))
                           labels)))

    (string-join temp-list ", ")))

(defun cc/gh-iso8601-to-local-org-time (timestamp)
  "Convert an ISO 8601 UTC TIMESTAMP to local Org timestamp."
  (let* ((time-components (parse-time-string timestamp))
         (utc-time (encode-time time-components))
         (local-time (current-time-zone utc-time)))
    (format-time-string "%Y-%m-%d %H:%M:%S" (apply 'encode-time time-components) local-time)))

;; ;; Example usage
;; (let ((utc-timestamp "2024-12-23T02:42:41Z"))
;;   (message "Local time: %s" (cc/gh-iso8601-to-local-org-time utc-timestamp)))



;; (defvar-keymap vtable-map
;;   "S" #'vtable-sort-by-current-column
;;   "{" #'vtable-narrow-current-column
;;   "}" #'vtable-widen-current-column
;;   "g" #'vtable-revert-command
;;   "M-<left>" #'vtable-previous-column
;;   "M-<right>" #'vtable-next-column)

(keymap-set vtable-map "TAB" #'vtable-next-column)
(keymap-set vtable-map "<backtab>" #'vtable-previous-column)

(defun cc/gh-browse-url (issue)
  "Open URL in ISSUE."
  (let* ((url (map-elt issue "url")))
    (browse-url url)))

(defun cc/gh-format-buffer-name (issue)
  "Generate buffer name from ISSUE."

  (let ((repo cc/gh-repo-name)
        (number (map-elt issue "number"))
        (title (map-elt issue "title")))
    (format "*%s: #%d %s*"
            repo
            number
            title)))

(defun cc/gh-copy-issue (issue)
  "Copy ISSUE to `kill-ring'."

  (let* ((bufname (cc/gh-format-buffer-name issue)))
    (kill-new (cc/gh-render-issue-as-org
               issue
               (string-trim (car cc/gh--last-repo-history))))
    (message "Copied %s to kill ring" bufname)))


(defun cc/gh-switch-to-issue ()
  "Switch to issue."
  (interactive)
  (let* ((issue (vtable-current-object))
         (bufname (cc/gh-format-buffer-name issue)))

    (if (get-buffer bufname)

        (select-window (get-buffer-window (switch-to-buffer-other-window bufname)))

      (cc/gh-browse-issue issue)
      (select-window (get-buffer-window (switch-to-buffer-other-window bufname))))))


(defun cc/gh-render-issue-as-org (issue repo)
  "Render ISSUE in REPO in Org format."

  (let* ((number (map-elt issue "number"))
         (title (map-elt issue "title"))
         (author (map-elt issue "author"))
         (assignees (map-elt issue "assignees"))
         (body (map-elt issue "body"))
         (url (map-elt issue "url"))
         (state (map-elt issue "state"))
         (labels (map-elt issue "labels"))
         (createdAt (map-elt issue "createdAt"))
         (updatedAt (map-elt issue "updatedAt"))
         (milestone (map-elt issue "milestone"))
         (created (cc/gh-iso8601-to-local-org-time createdAt))
         (updated (cc/gh-iso8601-to-local-org-time updatedAt))
         (temp-list ()))

    (push (format "** TODO %s #%d: %s" repo number title) temp-list)
    (push ":PROPERTIES:" temp-list)
    (push (format ":CREATED: %s" created) temp-list)
    (push (format ":UPDATED: %s" updated) temp-list)
    (if milestone
        (push (format ":MILESTONE: %s" (map-elt milestone "title")) temp-list))
    (if labels
        (push (format ":LABELS: %s" (cc/gh-format-labels (map-elt issue "labels"))) temp-list))

    (if assignees
        (push (format ":ASSIGNEES: %s"
                      (string-join
                       (mapcar (lambda (x) (map-elt x "name")) assignees) ", "))
              temp-list))

    (push (format ":STATE: %s" state) temp-list)
    (push (format ":AUTHOR: %s"
                  (map-elt author "name"))
          temp-list)

    (push ":END:" temp-list)
    (push "" temp-list)
    (push (format "[[%s][%s #%d: %s]]" url repo number title) temp-list)
    (push "" temp-list)
    (push (cc/gh-md2org body) temp-list)
    (push "" temp-list)
    (string-join (seq-reverse temp-list) "\n")))

(defun cc/gh-browse-issue (issue)
  "Browse ISSUE."

  (let* ((issue-window (selected-window))
         (repo cc/gh-repo-name)
         (body (cc/gh-render-issue-as-org issue repo))
         (bufname (cc/gh-format-buffer-name issue))
         (buf (get-buffer-create bufname)))

    (switch-to-buffer-other-window buf)

    (when (= (buffer-size) 0)
      (org-mode)
      (setq-local cc/gh-repo-name repo)
      (insert body)
      (read-only-mode))

    (select-window issue-window)))

(defface cc/gh-issues-face
  '((t (:inherit variable-pitch :extend t :height 0.9)))
  "Issues face.")

(defun cc/gh-next-line ()
  "Next line."
  (interactive)
  (forward-line 1)
  (cc/gh-browse-issue (vtable-current-object)))

(defun cc/gh-previous-line ()
  "Previous line."
  (interactive)
  (forward-line -1)
  (cc/gh-browse-issue (vtable-current-object)))

(defun cc/gh-request-issues (repo)
  "Request issues for REPO."

  (let ((cmd-list ())
        (fields '("number"
                  "title"
                  "body"
                  "author"
                  "assignees"
                  "url"
                  "state"
                  "labels"
                  "createdAt"
                  "updatedAt"
                  "milestone")))
    (push "gh" cmd-list)
    (push "--repo" cmd-list)
    (push (format "'%s'" repo) cmd-list)
    (push "issue" cmd-list)
    (push "list" cmd-list)
    (push "--limit 50" cmd-list)
    (push "--json" cmd-list)
    (push (string-join fields ",") cmd-list)

    (json-parse-string
     (shell-command-to-string
      (string-join (seq-reverse cmd-list) " "))
     :null-object nil)))

(defun cc/gh-refresh-issues ()
  "Refresh issues."
  (let* ((repo cc/gh-repo-name)
         (issues (cc/gh-request-issues repo)))
    (message "Refreshed %s issues (%d)" repo (length issues))
    (seq-into issues 'list)))


(defun cc/gh-kill-all-repo-buffers ()
  "Kill current repo buffers."
  (interactive)
  (let* ((repo cc/gh-repo-name)
         (pat (format "*%s" repo))
         (blist (buffer-list))
         (repo-buffers (seq-filter
                        (lambda (b)
                          (let ((bufname (buffer-name b)))
                            (string-match pat bufname)))
                        blist))
         )

    (mapc (lambda (b)
            (kill-buffer b))
          repo-buffers)
    (message "Killed all %s buffers" repo)))

(defun cc/gh-issues ()
  "Put current issues for a GitHub repository in a vtable.

The command prompts the user for a GitHub repository, which if it
exists will then retrieve the current list of issues for it via gh."
  (interactive)

  (let* ((repo (string-trim (cc/gh-read-repo "repo: ")))
         (repo-buffer-name (format "*issues: %s*" repo)))

    (get-buffer-create repo-buffer-name)
    (toggle-truncate-lines t)
    (switch-to-buffer (set-buffer repo-buffer-name))
    (setq-local cc/gh-repo-name repo)

    (read-only-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (make-vtable
       :columns '((:name "#")
                  (:name "Title" :width 40)
                  (:name "Author")
                  (:name "Assignees")
                  (:name "Labels")
                  (:name "Milestone" :primary ascend)
                  ;; (:name "Updated" :displayer (lambda (value max-width table)
                  ;;                               (propertize value 'face 'fixed-pitch)))
                  (:name "Updated")
                  (:name "Created"))

       :face 'cc/gh-issues-face

       :actions '("c" cc/gh-copy-issue
                  "<double-mouse-1>" cc/gh-browse-url)

       :objects-function #'cc/gh-refresh-issues

       :getter (lambda (issue column table)
                 (pcase (vtable-column table column)
                   ("#" (map-elt issue "number"))
                   ("Title" (map-elt issue "title"))
                   ("Author" (map-elt (map-elt issue "author") "name"))
                   ("Assignees" (string-join
                                 (mapcar (lambda (x) (map-elt x "name"))
                                         (map-elt issue "assignees"))
                                 ", "))
                   ("Labels" (cc/gh-format-labels (map-elt issue "labels")))
                   ("Milestone" (let ((milestone (map-elt issue "milestone")))
                                  (if milestone
                                      (map-elt milestone "title")
                                    "")))
                   ("Created" (cc/gh-iso8601-to-local-org-time (map-elt issue "createdAt")))
                   ("Updated" (cc/gh-iso8601-to-local-org-time (map-elt issue "updatedAt")))))
       :keymap (define-keymap
                 "RET" #'cc/gh-switch-to-issue
                 "q" #'quit-window
                 "n" #'cc/gh-next-line
                 "p" #'cc/gh-previous-line
                 "j" #'cc/gh-next-line
                 "t" #'toggle-truncate-lines
                 "k" #'cc/gh-previous-line
                 "C-o" #'cc/gh-issues-tmenu
                 "K" #'cc/gh-kill-all-repo-buffers)))))

(transient-define-prefix cc/gh-issues-tmenu ()
  "GitHub issues client menu."
  ["GitHub Issues"
   ["Actions"
    ("b" "Browse" (lambda ()
                    (interactive)
                    (cc/gh-browse-issue (vtable-current-object))))

    ("c" "Copy as Org" (lambda ()
                         (interactive)
                         (cc/gh-copy-issue (vtable-current-object))))

    ("K" "Kill All" cc/gh-kill-all-repo-buffers)]

   ["Navigation"
    ("p" "Up" previous-line :transient t)
    ("n" "Down" next-line :transient t)]

   ["View"
    ("t" "Toggle Truncate Lines" toggle-truncate-lines)
    ("g" "Refresh" vtable-revert-command)]])

(provide 'cc-gh)
;;; cc-gh.el ends here
