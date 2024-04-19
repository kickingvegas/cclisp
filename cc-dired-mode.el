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
(require 'org)
(require 'org-ql)
(require 'wdired)
(require 'cc-dired-sort-by)
(require 'cc-main-tmenu)

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'dired-async-mode)

(defun cc/dired-image-info ()
  "Message image info in the minibuffer and push into kill-ring."
  (interactive)
  (when (org-file-image-p (dired-get-filename))
    (let* ((filename (dired-get-filename))
           (image-info (cc/--image-info filename))
           (output (concat image-info
                           " "
                           (file-name-base filename)
                           "."
                           (file-name-extension filename))))
      (message output)
      (kill-new output))))

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

(transient-define-prefix cc/dired-tmenu ()
  "Transient menu for dired."
  ["Dired"
   ["File"
    ("C" "Copy to…" dired-do-copy :transient nil)
    ("R" "Rename…" dired-do-rename :transient t)
    ("S" "Symlink to…" dired-do-symlink :transient t)
    ("c" "Change›" cc/dired-change-tmenu :transient nil)
    ("w" "Copy Name" dired-copy-filename-as-kill :transient nil)
    ("!" "Shell Command" dired-do-shell-command :transient nil)
    ("&" "Shell Command ☕︎" dired-do-async-shell-command :transient nil)
    ("W" "Browse" browse-url-of-dired-file :transient nil)]

   ["Directory"
    ("s" "Sort By›" cc/dired-sort-by :transient t)
    ("h" "Hide Details" dired-hide-details-mode
     :description (lambda () (cc/--checkbox-label dired-hide-details-mode "Hide Details"))
     :transient t)
    ("o" "Omit Mode" dired-omit-mode
     :description (lambda () (cc/--checkbox-label dired-omit-mode "Omit Mode"))
     :transient t)
    ("i" "Insert Subdir" dired-maybe-insert-subdir :transient t)
    ("$" "Hide/Unhide Subdir" dired-hide-subdir :transient t)
    ("k" "Kill Line" dired-do-kill-lines :transient t)
    ("E" "Edit Filenames (wdired)" wdired-change-to-wdired-mode :transient nil)
    ("I" "Thumbnails…" image-dired :transient n)]

   ["Mark"
    ("t" "Toggle Marks" dired-toggle-marks :transient t)
    ("m" "Mark" dired-mark :transient t)
    ("u" "Unmark" dired-unmark :transient qt)
    ("U" "Unmark All" dired-unmark-all-marks :transient t)
    ("r" "Regexp›" cc/dired-regexp-tmenu :transient nil)]

   ["Navigation"
    :pad-keys t
    ("^" "Up Directory" dired-up-directory :transient t)
    ("p" "↑" dired-previous-line :transient t)
    ("n" "↓" dired-next-line :transient t)
    ("M-p" "↑ 📁" dired-prev-dirline :transient t)
    ("M-n" "↓ 📁" dired-next-dirline :transient t)
    ("[" "↑ 🗂️" dired-prev-subdir :transient t)
    ("]" "↓ 🗂️" dired-next-subdir :transient t)]]

  [["Quick"
    ("j" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)
    ("M" "Main Menu›" cc/main-tmenu :transient nil)]

   ["Search"
    ("C-s" "I-Search Filenames…" dired-isearch-filenames :transient nil)
    ("M-s" "I-Search Regexp Filenames…" dired-isearch-filenames-regexp :transient nil)]]

  ;; ["Menu"
  ;;  :class transient-row
  ;;   ("o" "Open›" cc/open-tmenu :transient nil)
  ;;   ("W" "Windows & Tabs›" cc/windows-tmenu :transient nil)
  ;;   ("e" "Edit›" cc/edit-tmenu :transient nil)
  ;;   ("B" "Bookmarks›" cc/bookmarks-tmenu :transient nil)
  ;;   ("s" "Search›" cc/search-tmenu :transient nil)
  ;;   ("T" "Tools›" cc/tools-tmenu :transient nil)]

  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/dired-regexp-tmenu ()
  "Transient menu for dired regexp."
  ["Regexp"
   ("m" "Mark Files…" dired-mark-files-regexp :transient nil)
   ("c" "Mark Files Containing…" dired-mark-files-containing-regexp :transient nil)
   ("d" "Mark Files For Deletion…" dired-flag-files-regexp :transient nil)
   ("C" "Mark Files To Copy…" dired-do-copy-regexp :transient nil)
   ("r" "Mark Files To Rename…" dired-do-rename-regexp :transient nil)]

  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/dired-change-tmenu ()
  ["Change"
   ("T" "Touch" dired-do-touch :transient t)
   ("M" "Mode…" dired-do-chmod :transient t)
   ("G" "Group…" dired-do-chgrp :transient t)
   ("O" "Owner…" dired-do-chown :transient t)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
