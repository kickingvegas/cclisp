;;; cc-gh.el --- gh commands                         -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

(defvar cc/gh--last-issue-numbers nil
  "Private variable to store last used GitHub issue numbers.")

(defun cc/gh-read-repo (prompt)
  "Prompt the user with PROMPT, using the last history entry as the default input."
  (let* ((history 'cc/gh--last-repo-history)  ; Define the history variable
         (last-history-entry (car (symbol-value history))))  ; Get the last entry
    (string-trim (read-string prompt last-history-entry history))))

(defun cc/gh-list-issues ()
  "Insert Org table containing current issues for a GitHub repository.

The command prompts the user for a GitHub repository, which if it
exists will then retrieve the current list of issues for it via gh."
  (interactive)

  (let* ((repo (cc/gh-read-repo "repo: "))
         (cmd-list (list))
         (output-list (list))
         (repo-buffer-name (format "*issues: %s*" repo)))
    (push "gh" cmd-list)
    (push "--repo" cmd-list)
    (push (format "'%s'" repo) cmd-list)
    (push "issue" cmd-list)
    (push "list" cmd-list)
    (push "--json" cmd-list)
    (push "number,title,url,state,labels,createdAt,updatedAt,milestone" cmd-list)

    (let* ((issues (json-parse-string
                    (shell-command-to-string
                     (string-join (seq-reverse cmd-list) " "))))
           (header (list '((:label . "#") (:span . 0))
                         '((:label . "Title") (:span . 40))
                         ;; '((:label . "State") (:span . 0))
                         '((:label . "Labels") (:span . 5))
                         ;; '((:label . "Milestone") (:span . 5))
                         '((:label . "Created") (:span . 11))
                         '((:label . "Updated") (:span . 11))))
           (header-labels (mapcar (lambda (x) (alist-get :label x)) header))
           (header-spans (mapcar (lambda (x) (alist-get :span x)) header))
           (numbers (list)))

      (mapc (lambda (issue)
              (let* ((number (gethash "number" issue))
                     (title (gethash "title" issue))
                     ;;(state (gethash "state" issue))
                     (labels (gethash "labels" issue))
                     (created (cc/gh-iso8601-to-local-org-time (gethash "createdAt" issue)))
                     (updated (cc/gh-iso8601-to-local-org-time (gethash "updatedAt" issue)))
                     (url (gethash "url" issue))
                     ;;(milestone (gethash "milestone" issue))
                     (temp-list (list)))

                (push (number-to-string number) numbers)
                (push (format "[[%s][%d]]" url number) temp-list)
                (push (format "[[%s][%s]]" url title) temp-list)
                ;; (push state temp-list)
                ;; (push milestone temp-list)
                (push (cc/gh-format-labels labels) temp-list)
                (push created temp-list)
                (push updated temp-list)

                (push
                 (format "| %s |\n" (string-join (seq-reverse temp-list) " | "))
                 output-list)))
            issues)

      (setq cc/gh--last-issue-numbers (reverse numbers))

      (get-buffer-create repo-buffer-name)
      (switch-to-buffer (set-buffer repo-buffer-name))
      (erase-buffer)
      (org-mode)
      (visual-line-mode 0)

      (insert "|")
      (mapc (lambda (x)
              (if (> x 0)
                  (insert (format "<%d>|" x))
                (insert " |")))
            header-spans)
      (insert "\n")

      (insert (format "| %s |\n" (string-join header-labels " | ")))
      (insert "|")
      (dotimes (_ (length header)) (insert "---|"))
      (insert "\n")

      (insert (string-join (seq-reverse output-list) ""))
      (goto-char (point-min))

      (org-table-next-field)
      (goto-char (point-min))

      (insert (format "* %s\n" repo)))))

(defun cc/gh-issue ()
  "Insert Org table issue for a GitHub repository.

The command prompts the user for a GitHub repository, which if it
exists will then retrieve the current list of issues for it via gh."
  (interactive)

  (let* ((repo (cc/gh-read-repo "repo: "))
         (id (if cc/gh--last-issue-numbers
                 (completing-read "Number: " cc/gh--last-issue-numbers)
               (read-string "Number: ")))
         (cmd-list (list)))
    (push "gh" cmd-list)
    (push "--repo" cmd-list)
    (push (format "'%s'" repo) cmd-list)
    (push "issue" cmd-list)
    (push "view" cmd-list)
    (push id cmd-list)
    (push "--json" cmd-list)
    (push "'number,title,url,body,createdAt'" cmd-list)

    (let* ((issue (json-parse-string
                    (shell-command-to-string
                     (string-join (seq-reverse cmd-list) " "))))
           (number (gethash "number" issue))
           (body (gethash "body" issue))
           (title (gethash "title" issue))
           (url (gethash "url" issue))
           (created (cc/gh-iso8601-to-local-org-time (gethash "createdAt" issue)))
           (temp-list (list)))
      (push (format "** TODO %s #%d: %s" repo number title) temp-list)
      (push ":PROPERTIES:" temp-list)
      (push (format ":CREATED: %s" created) temp-list)
      (push ":END:" temp-list)
      (push "" temp-list)
      (push (format "[[%s][%s #%d: %s]]" url repo number title) temp-list)
      (push "" temp-list)
      (push (cc/gh-md2org body) temp-list)
      (push "" temp-list)
      (insert (string-join (seq-reverse temp-list) "\n")))))

(defun cc/gh-md2org (buf)
  "Convert BUF text format from Markdown to Org."
  (save-excursion
    (with-temp-buffer
      (insert buf)
      (shell-command-on-region (point-min) (point-max) "pandoc --to=org --wrap=preserve" (current-buffer) t)
      (buffer-string))))

(defun cc/gh-format-labels (labels)
  "Convert LABELS to a comma-separated string.

LABELS is a vector of hash-tables, each hash-table corresponding
to the JSON dictionary containing label information returned by
gh."
  (if labels
      (let* ((temp-list (list)))
        (mapc (lambda (label)
                (let* ((name (map-elt label "name")))
                  (push name temp-list)))
              labels)

        (string-join  temp-list ", "))
      ""))

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

(defun cc/gh-browse-url (obj)
  (let* ((url (map-elt obj "url")))
    (browse-url url)))


(defun cc/gh-list-issues-vtable ()
  "Put current issues for a GitHub repository in a vtable.

The command prompts the user for a GitHub repository, which if it
exists will then retrieve the current list of issues for it via gh."
  (interactive)

  (let* ((repo (cc/gh-read-repo "repo: "))
         (cmd-list (list))
         (repo-buffer-name (format "*issues: %s*" repo)))
    (push "gh" cmd-list)
    (push "--repo" cmd-list)
    (push (format "'%s'" repo) cmd-list)
    (push "issue" cmd-list)
    (push "list" cmd-list)
    (push "--json" cmd-list)
    (push "number,title,url,state,labels,createdAt,updatedAt,milestone" cmd-list)

    (let* ((issues (json-parse-string
                    (shell-command-to-string
                     (string-join (seq-reverse cmd-list) " "))))
           ;; (header (list '((:label . "#") (:span . 0))
           ;;               '((:label . "Title") (:span . 40))
           ;;               ;; '((:label . "State") (:span . 0))
           ;;               '((:label . "Labels") (:span . 5))
           ;;               ;; '((:label . "Milestone") (:span . 5))
           ;;               '((:label . "Created") (:span . 11))
           ;;               '((:label . "Updated") (:span . 11))))

           ;;(header-labels (mapcar (lambda (x) (alist-get :label x)) header))
           ;;(header-spans (mapcar (lambda (x) (alist-get :span x)) header))
           ;; (numbers (list))
           )

      (unless (> (length issues) 0)
        (error (format "Repository %s has no issues." repo)))


      (get-buffer-create repo-buffer-name)
      (switch-to-buffer (set-buffer repo-buffer-name))

      (read-only-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (make-vtable
         :columns '((:name "#")
                    (:name "Title" :width 40)
                    (:name "Labels")
                    (:name "Updated" :primary descend)
                    (:name "Created"))

         :actions '("RET" cc/gh-browse-url
                    "<double-mouse-1>" cc/gh-browse-url)
         :objects (seq-into issues 'list)

         :getter (lambda (issue column table)
                   (pcase (vtable-column table column)
                     ("#" (map-elt issue "number"))
                     ("Title" (map-elt issue "title"))
                     ("Labels" (cc/gh-format-labels (map-elt issue "labels")))
                     ("Created" (cc/gh-iso8601-to-local-org-time (map-elt issue "createdAt")))
                     ("Updated" (cc/gh-iso8601-to-local-org-time (map-elt issue "updatedAt")))
                     ))
         :keymap (define-keymap
                   "q" #'quit-window))))))


(provide 'cc-gh)
;;; cc-gh.el ends here
