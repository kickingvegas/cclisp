;;; cc-main-tmenu.el --- Main Menu                    -*- lexical-binding: t; -*-

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
(require 'casual-editkit)
(require 'cclisp)
(require 'password-store-menu)
(require 'google-translate-smooth-ui)
(require 'webpaste)

(transient-define-prefix cc/main-tmenu ()
  "Main menu for Charles Choi."
  [["File"
    ("o" "Open›" casual-editkit-open-tmenu)
    ("f" "Open file…" find-file)
    ("d" "Open in Dired" dired-jump-other-window
     :if (lambda () (buffer-file-name)))
    ("b" "List Buffers" ibuffer)
    ("R" "Recent Files" recentf-open-files)
    ("j" "Goto Journal…" cc/select-journal-file)
    ("s" "Save" save-buffer)]

   ["Edit"
    :pad-keys t
    ("e" "Edit›" casual-editkit-edit-tmenu)
    ("p" "Fill Paragraph" fill-paragraph
     :if-not casual-editkit-buffer-read-only-p)
    ("l" "Join line" join-line
     :transient nil
     :if-not casual-editkit-buffer-read-only-p)
    ("C-o" "Open line" open-line
     :transient t
     :if-not casual-editkit-buffer-read-only-p)
    ("I" "Korean Input"
     (lambda () (interactive)(set-input-method 'korean-hangul))
     :transient nil)
    ("E" "Emoji & Symbols›" casual-editkit-emoji-symbols-tmenu
     :if-not casual-editkit-buffer-read-only-p)]

   ["Sexp"
    ("m" "Mark" mark-sexp)
    ("c" "Copy" casual-editkit-copy-sexp)
    ("k" "Kill (Cut)" kill-sexp
     :if-not casual-editkit-buffer-read-only-p)
    ("t" "Transpose" transpose-sexps
     :if-not casual-editkit-buffer-read-only-p)]

   ["Tools"
    ("T" "Tools›" cc/tools-tmenu)
    ("a" "Org Agenda" org-agenda)
    ("C" "Compile…" compile)
    ("g" "Magit Status" casual-editkit-select-magit-command
     :description casual-editkit-select-magit-command-description
     :if (lambda ()
           (and (casual-editkit-package-magit-installed-p)
                (casual-editkit-version-controlled-p))))
    ("h" "Highlight Symbol" casual-editkit-symbol-overlay-put
     :if casual-editkit-package-symbol-overlay-installed-p)]]

  [[;;"Bookmarks"
    ("B" "Bookmarks›" casual-editkit-bookmarks-tmenu)
    ("J" "Jump to Bookmark…" bookmark-jump)]

   [;;"Window"
    ("w" "Window›" casual-editkit-windows-tmenu)
    ("M-n" "New Frame" make-frame-command)]

   [;;"Search/Replace"
    ("/" "Search/Replace›" casual-editkit-search-tmenu)
    ("P" "Project›" casual-editkit-project-tmenu)]

   [("M" "Macros›" casual-editkit-macro-tmenu)]]

  ;; casual-editkit-cursor-navigation-group

  [:class transient-row
   (casual-lib-quit-one)
   ("r" "Registers›" casual-editkit-registers-tmenu)
   ("U" "Undo" undo :transient t)
   ("," "Settings›" casual-editkit-settings-tmenu)
   (casual-lib-quit-all)

   ("x" "Exit Emacs" save-buffers-kill-emacs)])


(transient-define-prefix cc/tools-tmenu ()
  "Menu for ‘Tools’ commands.

Tools specific to Charles Choi "
  ["Tools"
   ["Shells/REPLs"
    ("s" "Shell" shell)
    ("e" "Eshell" eshell)
    ("i" "IELM" ielm)
    ("t" "term" term)
    ("p" "Python" run-python)]

   ["Utilities"
    ("c" "Calc" calc)
    ("r" "RE-Builder" re-builder)
    ("w" "Word Count" (lambda () (interactive) (call-interactively #'count-words)))
    ("P" "Password›" password-store-menu)]

   ["Almanac"
    ("C" "World Clock" world-clock)
    ("S" "Sunrise/Sunset" sunrise-sunset)
    ("W" "Weather" weather)]

   ["Region"
    :if use-region-p
    ("C" "Call" cc/call-nanp-phone-number)
    ("m" "Maps" cc/open-region-in-apple-maps)
    ("M-s" "Say" cc/say-region)
    ("M-t" "Translate" google-translate-smooth-translate)
    ("M-p" "Webpaste" webpaste-paste-region)]

   ["Fun"
    ("T" "Tetris" tetris)
    ("z" "Zone" zone)
    ("F" "Fireplace" fireplace)
    ("Z" "Snow" snow)]]

  casual-editkit-navigation-group)

(provide 'cc-main-tmenu)
;;; cc-main-tmenu.el ends here
