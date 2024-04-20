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
(require 'transient)
(require 'bookmark)
(require 'cclisp)
(require 'org-agenda)

(defun cc/version-controlled-p ()
  "Predicate if version controlled."
  (vc-responsible-backend default-directory t))

(defun cc/find-dired-regexp (REGEXP)
  "Find files in currentx directory whose names match REGEXP."
  (interactive "sFind filenames with regex: ")
  (find-lisp-find-dired default-directory REGEXP))

(defun cc/prog-mode-p ()
  "Predicate if current buffer is `prog-mode'."
  (derived-mode-p 'prog-mode))

(defun cc/org-mode-p ()
  "Predicate if current buffer is `org-mode'."
  (derived-mode-p 'org-mode))

(defun cc/org-agenda-all-todos ()
  "Invoke `org-agenda' to show all TODO items."
  (interactive)
  (org-agenda nil "n"))

(transient-define-prefix cc/main-tmenu ()
  "CC Main Menu."
  [["Quick"
    ("f" "Find Dired…" cc/find-dired-regexp :transient nil)
    ("j" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("a" "Org Agenda" cc/org-agenda-all-todos :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)
    ("w" "Jump to Window…" ace-select-window :transient nil)
    ("u" "URLs…" cc/open-url :if display-graphic-p :transient nil)
    ("R" "Recent Files" recentf-open-files :transient nil)
    ("J" "Journal Files" cc/select-journal-file :transient nil)
    ("P" "Switch to Project…" project-switch-project :transient nil)]

   ["Sexp"
     ("m" "Mark" mark-sexp :transient nil)
     ("c" "Copy" cc/copy-sexp :transient nil)
     ("k" "Kill" kill-sexp :transient nil)
     ("t" "Transpose" transpose-sexps :transient nil)]

   ["Edit"
    :pad-keys t
    ("i" "Insert Character…" insert-char :transient nil)
    ("p" "Fill Paragraph" fill-paragraph :transient nil)
    ("l" "Join line" join-line :transient nil)
    ("I" "Korean Input"
     (lambda () (interactive)(set-input-method 'korean-hangul))
     :transient nil)]

   ["Misc"
    :pad-keys t
    ("d" "Dired…" dired :transient nil)
    ("M-d" "Dired Other…" dired-other-window :transient nil)
    ("g" "Magit Status" magit :if cc/version-controlled-p :transient nil)
    ("D" "Ediff Revision" cc/ediff-revision
     :if cc/version-controlled-p :transient nil)
    ("C" "Compile…" compile :transient nil)
    ("r" "Registers" cc/registers-tmenu :transient nil)]]

  ["Menu"
   :class transient-row
    ("o" "Open›" cc/open-tmenu :transient nil)
    ("W" "Windows & Tabs›" cc/windows-tmenu :transient nil)
    ("e" "Edit›" cc/edit-tmenu :transient nil)
    ("B" "Bookmarks›" cc/bookmarks-tmenu :transient nil)
    ("s" "Search›" cc/search-tmenu :transient nil)
    ("T" "Tools›" cc/tools-tmenu :transient nil)]

  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/open-tmenu ()
  ["Open"
   ["File"
   ("f" "Fuzzy Find…" helm-find-files :transient nil)
   ("F" "File…" find-file :transient nil)]

   ["Project"
    ("p" "File in Project…" project-find-file :transient nil)
    ("d" "Project Dired…" project-dired :transient nil)
    ("s" "Switch to Project…" project-switch-project :transient nil)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-tmenu ()
  ["Edit"
   [("m" "Mark›" cc/edit-mark-tmenu :transient nil)
    ("c" "Copy›" cc/edit-copy-tmenu :transient nil)
    ("k" "Kill›" cc/edit-kill-tmenu :transient nil)]

   [("t" "Transpose›" cc/edit-transpose-tmenu :transient nil)
    ("v" "Move Text›" cc/edit-move-text-tmenu :transient nil)
    ("d" "Delete Space›" cc/edit-delete-space-tmenu :transient nil)]

   [("f" "Fill Paragraph›" fill-paragraph :transient nil)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-mark-tmenu ()
  ["Mark"
   ("w" "Word"  mark-word :transient nil)
   ("s" "Sentence" mark-end-of-sentence :transient nil)
   ("p" "Paragraph" mark-paragraph :transient nil)
   ("b" "Balanced Expression (sexp)" mark-sexp :transient nil)
   ("d" "Defun" mark-defun :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-copy-tmenu ()
  ["Copy"
   [("w" "Word" cc/copy-word :transient nil)
    ("s" "Sentence" cc/copy-sentence :transient nil)
    ("p" "Paragraph" cc/copy-paragraph :transient nil)]
   [("b" "Balanced Expression (sexp)" cc/copy-sexp :transient nil)
    ("d" "Defun" cc/copy-defun :transient nil)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-kill-tmenu ()
  ["Kill"
   ("w" "Word"  kill-word :transient nil)
   ("l" "Line" kill-line :transient nil)
   ("s" "Sentence" kill-sentence :transient nil)
   ("p" "Paragraph" kill-paragraph :transient nil)
   ("b" "Balanced Expression (sexp)" kill-sexp :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-transpose-tmenu ()
  ["Transpose"
   [("c" "Characters"  transpose-chars :transient nil)
    ("w" "Words"  transpose-words :transient nil)
    ("l" "Lines" transpose-lines :transient nil)
    ("s" "Sentences" transpose-sentences :transient nil)]
   [("p" "Paragraphs" transpose-paragraphs :transient nil)
    ("b" "Balanced Expression (sexp)" transpose-sexps :transient nil)
    ("r" "Regions" transpose-regions :transient nil)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-delete-space-tmenu ()
  ["Delete Space"
   [("o" "Just One Space" just-one-space :transient nil)
    ("j" "Join Line" join-line :transient nil)
    ("h" "Horizontal Space" delete-horizontal-space :transient nil)]

   [("b" "Blank Lines" delete-blank-lines :transient nil)
    ("w" "Whitespace Cleanup" whitespace-cleanup :transient nil)
    ("d" "Delete Trailing Whitespace" delete-trailing-whitespace :transient nil)]]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-move-text-tmenu ()
  ["Move Text"
   ("w" "Word"  cc/edit-move-word-tmenu :transient nil)
   ("s" "Sentence"  cc/edit-move-sentence-tmenu :transient nil)
   ("b" "Balanced Expression" cc/edit-move-sexp-tmenu :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-move-word-tmenu ()
  ["Move Word"
   :class transient-row
   ("b" "Backward"  cc/move-word-backward :transient t)
   ("f" "Forward"  cc/move-word-forward :transient t)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-move-sentence-tmenu ()
  ["Move Sentence"
   :class transient-row
   ("b" "Backward"  cc/move-sentence-backward :transient t)
   ("f" "Forward"  cc/move-sentence-forward :transient t)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/edit-move-sexp-tmenu ()
  ["Move Sexp"
   :class transient-row
   ("b" "Backward"  cc/move-sexp-backward :transient t)
   ("f" "Forward"  cc/move-sexp-forward :transient t)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/windows-tmenu ()
  ["Windows"
   ("j" "Jump to Window…" ace-select-window :transient nil)
   ("s" "Swap" window-swap-states :transient nil)
   ("t" "Transpose" transpose-frame :transient nil)
   ("b" "New Window Below" split-window-below :transient nil)
   ("r" "New Window on Right" split-window-horizontally :transient nil)
   ;;("T" "Toggle Tab Bar" mac-toggle-tab-bar :if ma :transient nil)
   ]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/bookmarks-tmenu ()
  ["Bookmarks"
   ("e" "Edit Bookmarks" cc/list-bookmarks-transient :transient nil)
   ("a" "Add Bookmark…" bookmark-set-no-overwrite :transient nil)
   ("j" "Jump to Bookmark…" bookmark-jump :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/search-tmenu ()
  ["Search"
    ("r" "Find in Files (rgrep)" rgrep :transient nil)
    ("s" "Spotlight" spotlight-fast :transient nil)
    ("o" "Org Files" cc/org-search :transient nil)
    ("g" "Google" google-this-search :if display-graphic-p :transient nil)
    ("m" "Apple Maps" cc/apple-maps-search :if display-graphic-p :transient nil)
    ("Q" "Org QL Search" org-ql-search :transient nil)]
  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/tools-tmenu ()
  ["Tools"
   ["Shells & Interpreters"
    ("e" "eshell" eshell :transient nil)
    ("i" "IELM" ielm :transient nil)
    ("s" "shell" shell :transient nil)
    ("t" "term" term :transient nil)
    ("p" "Python" run-python :transient nil)]

   ["Utilities"
    ("c" "Calc" calc :transient nil)
    ("W" "Weather" weather :transient nil)
    ("r" "RE-Builder" re-builder :transient nil)
    ("w" "Word Count" count-words :transient nil)
    ("C" "World Clock" world-clock :transient nil)]

   ["Fun"
    ("z" "Zone" zone :transient nil)]]

  [("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/registers-tmenu ()
  ["Registers"
   ["Store"
    ("p" "Record Point…" point-to-register :transient nil)
    ("w" "Window Configuration…" window-configuration-to-register :transient nil)
    ("m" "Keyboard Macro…" kmacro-to-register :transient nil)
    ("j" "Jump…" jump-to-register :transient nil)]

   ["Store Text"
    ("c" "Copy Region…" copy-to-register :transient nil)
    ("r" "Copy Rectangle…" copy-rectangle-to-register :transient nil)
    ("a" "Append to Register…" append-to-register :transient nil)
    ("P" "Prepend to Register…" prepend-to-register :transient nil)
    ("i" "Insert Text…" insert-register :transient nil)]]

  [("q" "Dismiss" ignore :transient transient--do-exit)])

(provide 'cc-main-tmenu)
;;; cc-main-tmenu.el ends here
