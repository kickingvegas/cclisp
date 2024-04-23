;;; cc-dired-mode.el --- Dired Customization -*- lexical-binding: t -*-

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
;;

;;; Code:
(require 'dired)
(require 'dired-x)
(require 'cclisp)
(require 'helm)
(require 'wdired)
(require 'cc-dired-sort-by)
(require 'image-dired)

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'dired-async-mode)

(defun cc/dired-image-info ()
  "Message image info in the minibuffer and push into kill-ring."
  (interactive)
  (if (cc/dired-image-file-p)
    (let* ((filename (dired-get-filename))
           (image-info (cc/--image-info filename))
           (output (concat image-info
                           " "
                           (file-name-base filename)
                           "."
                           (file-name-extension filename))))
      (message output)
      (kill-new output))
    (message "Not an image file.")))

(defun cc/find-lisp-dired-buffer-p ()
  "Predicate if buffer name is “*Find Lisp Dired*”."
  (string-equal (buffer-name) "*Find Lisp Dired*"))

(defun cc/dired-inspect-object ()
  "WIP: inspect Dired object."
  (interactive)
  (if (dired-get-subdir)
      ;; nop
      (message "subdir")
    (let ((fname (dired-get-filename)))
      (cond
       ((not (derived-mode-p 'dired-mode))
        ;; nop
        (message "Not in dired mode."))

       ((file-symlink-p fname)
        ;; nop
        (message fname))

       ((file-directory-p fname)
        ;; open in subdir
        (message "directory")
        (dired-maybe-insert-subdir fname))

       ((file-regular-p fname)
        ;; mark file
        (message fname)
        (dired-find-file))

       (t
        (message "undefined"))))))

;; (add-hook 'dired-mode-hook (lambda ()
;;                              (define-key dired-mode-map (kbd "<mouse-1>") 'cc/dired-inspect-object)))

(add-hook
 'dired-mode-hook
 (lambda ()
   (setq-local mouse-1-click-follows-link 'double)))

(define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
(define-key dired-mode-map (kbd "C-o") 'cc/dired-tmenu)
(define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "s") 'cc/dired-sort-by)
(define-key dired-mode-map (kbd "j") 'helm-find-files)
(define-key dired-mode-map (kbd "M-n") 'dired-next-dirline)
(define-key dired-mode-map (kbd "M-p") 'dired-prev-dirline)
(define-key dired-mode-map (kbd "]") 'dired-next-subdir)
(define-key dired-mode-map (kbd "[") 'dired-prev-subdir)
(define-key dired-mode-map (kbd "A-M-<mouse-1>") 'browse-url-of-dired-file)
;;(define-key dired-mode-map (kbd "A-M-<mouse-2>") 'cc/dired-inspect-object)

(define-key image-dired-thumbnail-mode-map (kbd "n") 'image-dired-display-next)
(define-key image-dired-thumbnail-mode-map (kbd "p") 'image-dired-display-previous)

(transient-define-prefix cc/dired-tmenu ()
  "Transient menu for dired."
  ["Dired"
   ["File"
    ("o" "Open Other" dired-find-file-other-window :transient nil)
    ("C" "Copy to…" dired-do-copy :transient nil)
    ("R" "Rename…" dired-do-rename :transient nil)
    ("D" "Delete…" dired-do-delete :transient nil)
    ("S" "Symlink…" dired-do-symlink :transient nil)
    ("c" "Change›" cc/dired-change-tmenu :transient nil)
    ("w" "Copy Name" dired-copy-filename-as-kill :transient nil)
    ("!" "$ …" dired-do-shell-command :transient nil)
    ("&" "$ &… " dired-do-async-shell-command :transient nil)
    ("W" "Browse" browse-url-of-dired-file :transient nil)]

   ["Directory"
    ("s" "Sort By›" cc/dired-sort-by :if-not cc/find-lisp-dired-buffer-p :transient t)
    ("h" "Hide Details" dired-hide-details-mode
     :description (lambda () (cc/--checkbox-label dired-hide-details-mode "Hide Details"))
     :if-not cc/find-lisp-dired-buffer-p
     :transient t)
    ("O" "Omit Mode" dired-omit-mode
     :description (lambda () (cc/--checkbox-label dired-omit-mode "Omit Mode"))
     :transient t)
    ("i" "Insert Subdir" dired-maybe-insert-subdir
     :if-not cc/find-lisp-dired-buffer-p
     :transient t)
    ("$" "Hide/Unhide Subdir" dired-hide-subdir
     :if-not cc/find-lisp-dired-buffer-p
     :transient t)
    ("k" "Kill (Hide) Line(s)" dired-do-kill-lines :transient t)
    ("g" "Revert" revert-buffer :transient t)
    ("f" "Filter…" cc/find-dired-regexp :transient nil)
    ("E" "Edit (wdired)" wdired-change-to-wdired-mode :transient nil)
    ("T" "Thumbnails…" image-dired :if display-graphic-p :transient n)
    ("I" "Image Info" cc/dired-image-info :transient t)]

   ["Mark"
    ("t" "Toggle Marks" dired-toggle-marks :transient t)
    ("m" "Mark" dired-mark :transient t)
    ("u" "Unmark" dired-unmark :transient t)
    ("U" "Unmark All" dired-unmark-all-marks :transient t)
    ("r" "Regexp›" cc/dired-regexp-tmenu :transient nil)]

   ["Navigation"
    :pad-keys t
    ("^" "᳞ 📁" dired-up-directory :transient t)
    ("p" "↑ 📄" dired-previous-line :transient t)
    ("n" "↓ 📄" dired-next-line :transient t)
    ("M-p" "↑ 📁" dired-prev-dirline :transient t)
    ("M-n" "↓ 📁" dired-next-dirline :transient t)
    ("[" "↑ 🗂️" dired-prev-subdir :transient t)
    ("]" "↓ 🗂️" dired-next-subdir :transient t)]]

  [["Quick"
    ("j" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)]

   ["Search"
    ("C-s" "I-Search…" dired-isearch-filenames :transient nil)
    ("M-s" "I-Search Regexp…" dired-isearch-filenames-regexp :transient nil)]

   ["New"
    ("+" "Directory" dired-create-directory :transient t)
    ("F" "File" dired-create-empty-file :transient t)]]

  [:class transient-row
   ("<return>" "Open" dired-find-file :transient nil)
   ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/dired-regexp-tmenu ()
  "Transient menu for dired regexp."
  ["Regexp Mark"
   ("m" "Files…" dired-mark-files-regexp :transient nil)
   ("c" "Files Containing…" dired-mark-files-containing-regexp :transient nil)
   ("d" "Files For Deletion…" dired-flag-files-regexp :transient nil)
   ("C" "Files To Copy…" dired-do-copy-regexp :transient nil)
   ("r" "Files To Rename…" dired-do-rename-regexp :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/dired-change-tmenu ()
  ["Change"
   [("M" "Mode…" dired-do-chmod :transient t)
    ("G" "Group…" dired-do-chgrp :transient t)
    ("O" "Owner…" dired-do-chown :transient t)]
   [("T" "Touch" dired-do-touch :transient t)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
