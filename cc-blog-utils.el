;;; cc-blog-utils.el --- Pelican Blog Utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'cclisp)
(require 'yasnippet)
(require 'org)
(require 'ox-gfm)


;; -------------------------------------------------------------------
;; Misc Functions

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

(defun cc/pelican-fix-image-src-refs (start end)
  "Fix HTML image src references in region bounded by START and END."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))

  (let* ((pat "src=\\([\\\"']\\)\\(images/.*\\)\\([\\\"']\\)")
         (rpat "src=\\1{static}\\2\\3"))
    (save-excursion
      (replace-regexp-in-region pat rpat start end))))

(defun cc/convert-md-image-to-html (start end)
  "Convert Markdown image to HTML in region bounded by START and END."
  (interactive "r")
  (unless (use-region-p)
    (error "No region selected"))

  (let* ((pat "\\(!\\[img\\]\\)(\\(images/.*\\))")
         (rpat "<p align='center'>\n<img src='{static}\\2' alt='' />\n</p>"))
    (save-excursion
      (replace-regexp-in-region pat rpat start end))))

(defun cc/markdown-insert-src-cookie ()
  "Insert Markdown source block cookie."
  (interactive)
  (let ((lang (completing-read "Language: " '("elisp"
                                               "python"
                                               "swift"
                                               "javascript"
                                               "c"
                                               "objc"
                                               "java") nil nil "elisp")))
    (insert (concat "\n    " "#!" lang))))


;; -------------------------------------------------------------------
;; Workflow Functions

(defun cc/blog-draft-post ()
  "Create draft post for ‘notes from /dev/null’ blog."
  (interactive)
  (let* ((title (read-string "Title: "))
         (slug (replace-regexp-in-string
                "[^a-z0-9-]" ""
                (replace-regexp-in-string
                 "\s+" "-"
                 (downcase title))))

         (datestamp (org-read-date))
         (filename (concat slug ".org"))
         (default-directory "~/org/posts")
         (image-dir (read-string "Image Directory: ")))

    (if (not (string-equal image-dir ""))
        (make-directory (file-name-concat default-directory "images" image-dir)))

    (switch-to-buffer (create-file-buffer filename))
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+AUTHOR: %s\n" "Charles Choi"))
    (insert (format "#+DATE: <%s>\n" datestamp))
    (insert (format "#+SUMMARY: %s\n" "This is a summary."))
    (insert (format "#+TAGS: %s\n" "emacs, org mode"))
    (insert "\n")
    (insert "Title: {{{title}}}\n\n")
    (insert "Date: {{{DATE(%Y-%m-%d %H:%M)}}}\n\n")
    (insert (format "Slug: %s\n\n" slug))
    (insert "Author: {{{author}}}\n\n")
    (insert "Summary: {{{keyword(SUMMARY)}}}\n\n")
    (insert "Tags: {{{keyword(TAGS)}}}\n\n")
    (save-buffer)))

(defun cc/blog-stage-post ()
  "Create blog stage post for Pelican."
  (interactive)
  (org-gfm-export-as-markdown)
  (switch-to-buffer "*Org GFM Export*")
  (let* ((content (buffer-substring (point-min) (point-max)))
         (pat "src=\\([\\\"']\\)\\(images/.*\\)\\([\\\"']\\)")
         (rpat "src=\\1{static}\\2\\3")
         (content (replace-regexp-in-string pat rpat content))
         (target-name (format-time-string "nfdn_%Y_%m_%d_%H%M%S.md")))
    (cd "~/Projects/devnull/content")
    (find-file target-name)
    (insert content)
    (goto-char (point-min))
    (re-search-forward "^Tags:")
    (flush-lines "^$" (point-min) (point) t)))


;; -------------------------------------------------------------------
;; Pelican Server Functions

(defun cc/launch-pelican ()
  "Launch a local instance of the Pelican static site server.
This function presumes that the buffer *pelican* is in the correct directory."
  (interactive)
  (process-send-string (get-buffer-process "*pelican*") "make devserver\n")
  (sleep-for 3)
  (shell-command "open http://localhost:8000"))

(defun cc/devserver ()
  "Open Pelican devserver for website chosen by completing read."
  (interactive)
  (let* ((choice (completing-read "Server: " '("devnull" "captee" "scrim")
                                  nil nil "devnull"))
         (blog-path (concat "~/Projects/pelican/" choice))
         (blog-buffer (format "*pelican-%s*" choice))
         (cd-blog-path (format "cd %s\n" blog-path)))

    (if (get-buffer blog-buffer)
        (switch-to-buffer blog-buffer)
      (progn
        (shell-new)
        (rename-buffer blog-buffer)
        (process-send-string (get-buffer-process blog-buffer) "cd ~/Projects/pelican\n")
        (process-send-string (get-buffer-process blog-buffer) "source .venv/bin/activate\n")
        (process-send-string (get-buffer-process blog-buffer) cd-blog-path)
        (setq-local default-directory blog-path)
        (if (display-graphic-p)
            (cc/launch-pelican))))))


(provide 'cc-blog-utils)

;;; cc-blog-utils.el ends here
