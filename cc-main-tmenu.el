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
(require 'ace-window)
(require 'recentf)
(require 'org-ql-search)
(require 'magit-status)
(require 'magit-files)
(require 'google-this)
(require 'symbol-overlay)
(require 'casual-lib)

(defun cc/version-controlled-p ()
  "Predicate if version controlled."
  (vc-responsible-backend default-directory t))

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

(defun cc/select-magit-command ()
  "Select appropriate Magit command given context."
  (interactive)
  (if (cc/version-controlled-p)
      (cond
       ((derived-mode-p 'dired-mode) (funcall-interactively #'magit-status))
       ((or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
        (funcall-interactively #'magit-file-dispatch))
       (t (funcall-interactively #'magit-status)))

    (message "Not a version controlled buffer.")))

(defun cc/select-magit-command-description ()
  "Select appropriate Magit command description given context."
  (if (cc/version-controlled-p)
      (cond
       ((derived-mode-p 'dired-mode) "Magit Status")
       ((or (derived-mode-p 'prog-mode)
            (derived-mode-p 'text-mode))
        "Magit Dispatch")
       (t "Magit Status"))
    (message "Not a version controlled buffer.")))

(transient-define-prefix cc/main-tmenu ()
  "CC Main Menu."
  ["Menu"
   :class transient-row
   ("o" "Open›" cc/open-tmenu :transient nil)
      ;; TODO: test buffer-read-only
   ("e" "Edit›" cc/edit-tmenu :transient nil)
   ("w" "Window›" cc/windows-tmenu :transient nil)
   ("B" "Bookmarks›" cc/bookmarks-tmenu :transient nil)
   ("s" "Search›" cc/search-tmenu :transient nil)
   ("T" "Tools›" cc/tools-tmenu :transient nil)]

  [["Quick"
    :pad-keys t
    ("f" "Open file…" find-file :transient nil)
    ("J" "Jump to Bookmark…" bookmark-jump :transient nil)
    ("b" "List Buffers" ibuffer :transient nil)
    ("R" "Recent Files" recentf-open-files :transient nil)
    ("j" "Journal Files" cc/select-journal-file :transient nil)
    ("a" "Org Agenda" cc/org-agenda-all-todos :transient nil)
    ("u" "URLs…" cc/open-url :if display-graphic-p :transient nil)]

   ["Sexp"
    ("m" "Mark" mark-sexp :transient nil)
    ;; TODO: test buffer-read-only
    ("c" "Copy" cc/copy-sexp :transient nil)
    ("k" "Kill" kill-sexp :transient nil)
    ("t" "Transpose" transpose-sexps :transient nil)]

   ["Edit"
    ;; TODO: test buffer-read-only
    :pad-keys t
    ("i" "Insert Character…" insert-char :transient nil)
    ("p" "Fill Paragraph" fill-paragraph :transient nil)
    ("l" "Join line" join-line :transient nil)
    ("C-o" "Newline" newline :transient nil)
    ("h" "Toggle Symbol Highlight" symbol-overlay-put)
    ("I" "Korean Input"
     (lambda () (interactive)(set-input-method 'korean-hangul))
     :transient nil)]

   ["Misc"
    :pad-keys t
    ("C" "Compile…" compile :transient nil)
    ("d" "Open in Dired" dired-jump-other-window
     :if (lambda () (buffer-file-name))
     :transient nil)
    ("g" "Magit Status" cc/select-magit-command
     :description cc/select-magit-command-description
     :if cc/version-controlled-p :transient nil)
    ("D" "Ediff Revision" cc/ediff-revision
     :if cc/version-controlled-p :transient nil)
    ("M-n" "New Frame" make-frame-command)]]

  [:class transient-row
          ("r" "Registers›" cc/registers-tmenu :transient nil)
          (casual-lib-quit-all)])

(transient-define-prefix cc/open-tmenu ()
  ["Open"
   ["File"
   ("f" "File…" find-file :transient nil)
   ("F" "Fuzzy Find…" helm-find-files :transient nil)]

   ["Project"
    ("p" "File in Project…" project-find-file :transient nil)
    ("d" "Project Dired…" project-dired :transient nil)
    ("P" "Switch to Project…" project-switch-project :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-tmenu ()
  ["Edit"
   [("m" "Mark›" cc/edit-mark-tmenu :transient nil)
    ("c" "Copy›" cc/edit-copy-tmenu :transient nil)
    ("k" "Kill›" cc/edit-kill-tmenu :transient nil)
    ("D" "Duplicate" duplicate-dwim :transient t)]

   [("t" "Transpose›" cc/edit-transpose-tmenu :transient nil)
    ("T" "Transform›" cc/transform-text-tmenu)
    ("v" "Move›" cc/edit-move-text-tmenu :transient nil)
    ("d" "Delete›" cc/edit-delete-space-tmenu :transient nil)]

   [:pad-keys t
    ("r" "Replace" query-replace :transient nil)
    ("M-r" "Replace Regexp" query-replace-regexp :transient nil)
    ("F" "Flush Lines…" flush-lines)
    ("K" "Keep Lines…" keep-lines)]

   [("f" "Fill Paragraph" fill-paragraph :transient nil)
    ("R" "Rectangle›" cc/rectangle-tmenu :transient nil)]]
  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(transient-define-prefix cc/edit-mark-tmenu ()
  ["Mark"
   [("w" "Word"  mark-word :transient nil)
    ("s" "Sentence" mark-end-of-sentence :transient nil)
    ("p" "Paragraph" mark-paragraph :transient nil)]
   [("b" "Balanced Expression (sexp)" mark-sexp :transient nil)
    ("d" "Defun" mark-defun :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-copy-tmenu ()
  ["Copy"
   [("w" "Word" cc/copy-word :transient nil)
    ("s" "Sentence" cc/copy-sentence :transient nil)
    ("p" "Paragraph" cc/copy-paragraph :transient nil)]
   [("b" "Balanced Expression (sexp)" cc/copy-sexp :transient nil)
    ("d" "Defun" cc/copy-defun :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-kill-tmenu ()
  ["Kill"
   [("w" "Word"  kill-word :transient nil)
    ("s" "Sentence" kill-sentence :transient nil)
    ("p" "Paragraph" kill-paragraph :transient nil)]
   [("l" "Line" kill-line :transient nil)
    ("b" "Balanced Expression (sexp)" kill-sexp :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-transpose-tmenu ()
  ["Transpose"
   [("c" "Characters" transpose-chars :transient nil)
    ("w" "Words" transpose-words :transient nil)
    ("l" "Lines" transpose-lines :transient nil)
    ("s" "Sentences" transpose-sentences :transient nil)]
   [("p" "Paragraphs" transpose-paragraphs :transient nil)
    ("b" "Balanced Expression (sexp)" transpose-sexps :transient nil)
    ("r" "Regions" transpose-regions :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-delete-space-tmenu ()
  ["Delete Space"
   [("o" "Just One Space" just-one-space :transient nil)
    ("j" "Join Line" join-line :transient nil)
    ("h" "Horizontal Space" delete-horizontal-space :transient nil)]

   [("b" "Blank Lines" delete-blank-lines :transient nil)
    ("w" "Whitespace Cleanup" whitespace-cleanup :transient nil)
    ("d" "Delete Trailing Whitespace" delete-trailing-whitespace :transient nil)]]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-move-text-tmenu ()
  ["Move Text"
   ("w" "Word›"  cc/edit-move-word-tmenu :transient nil)
   ("s" "Sentence›"  cc/edit-move-sentence-tmenu :transient nil)
   ("b" "Balanced Expression (sexp)›" cc/edit-move-sexp-tmenu :transient nil)]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-move-word-tmenu ()
  ["Move Word"
   :class transient-row
   ("b" "Backward"  cc/move-word-backward :transient t)
   ("f" "Forward"  cc/move-word-forward :transient t)]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-move-sentence-tmenu ()
  ["Move Sentence"
   :class transient-row
   ("b" "Backward"  cc/move-sentence-backward :transient t)
   ("f" "Forward"  cc/move-sentence-forward :transient t)]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/edit-move-sexp-tmenu ()
  ["Move Sexp"
   :class transient-row
   ("b" "Backward"  cc/move-sexp-backward :transient t)
   ("f" "Forward"  cc/move-sexp-forward :transient t)]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/windows-tmenu ()
  ["Window"
   ["Navigate"
    :pad-keys t
    ("o" "»" other-window :transient t)
    ("p" "↑" windmove-up :transient t)
    ("n" "↓" windmove-down :transient t)
    ("b" "←" windmove-left :transient t)
    ("f" "→" windmove-right :transient t)]

   ["Swap"
    :pad-keys t
    ("s" "⇄" window-swap-states :transient nil)
    ("M-p" "↑" windmove-swap-states-up :transient nil)
    ("M-n" "↓" windmove-swap-states-down :transient nil)
    ("M-b" "←" windmove-swap-states-left :transient nil)
    ("M-f" "→" windmove-swap-states-right :transient nil)]

   ["New"
    ("1" "❏" delete-other-windows :transient nil)
    ("2" "⇩" split-window-below :transient nil)
    ("3" "⇨" split-window-horizontally :transient nil)]

   ["Misc"
    ("t" "Transpose" transpose-frame :transient nil)
    ("T" "Toggle Tab Bar" mac-toggle-tab-bar
     :if window-system-mac-p :transient nil)
    ("J" "Jump to Window…" ace-select-window :transient nil)
    ("d" "Delete›" cc/windows-delete-tmenu :transient nil)]]

  ["Resize"
   ["↕︎"
    ("+" "Enlarge" enlarge-window :transient t)
    ("-" "Shrink" shrink-window :transient t)]
   ["↔︎"
    (">" "Enlarge" enlarge-window-horizontally :transient t)
    ("<" "Shrink" shrink-window-horizontally :transient t)]]

  [(casual-lib-quit-all)])

(transient-define-prefix cc/windows-delete-tmenu ()
  ["Delete Window"
   ("p" "Above" windmove-delete-up :transient nil)
   ("n" "Below" windmove-delete-down :transient nil)
   ("b" "On Left" windmove-delete-left :transient nil)
   ("f" "On Right" windmove-delete-right :transient nil)]
  [(casual-lib-quit-all)])

(defun window-system-mac-p ()
  "Predicate if window system is mac."
  (eq window-system 'mac))

(transient-define-prefix cc/bookmarks-tmenu ()
  ["Bookmarks"
   ("e" "Edit Bookmarks" cc/list-bookmarks-transient :transient nil)
   ("a" "Add Bookmark…" bookmark-set-no-overwrite :transient nil)
   ("J" "Jump to Bookmark…" bookmark-jump :transient nil)]
  [(casual-lib-quit-all)])

(transient-define-prefix cc/search-tmenu ()
  ["Search"
   ["Local"
    ("r" "Find in Files (rgrep)" rgrep :transient nil)
    ("s" "Spotlight" spotlight-fast :transient nil)
    ("o" "Org Files" cc/org-search :transient nil)
    ("Q" "Org QL Search" org-ql-search :transient nil)]

   ["Web"
    :if display-graphic-p
    ("g" "Google" google-this-search :transient nil)
    ("m" "Apple Maps" cc/apple-maps-search :transient nil)]]

  [(casual-lib-quit-all)])

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
    ("w" "Word Count" (lambda () (interactive) (call-interactively #'count-words)) :transient nil)
    ("C" "World Clock" world-clock :transient nil)
    ("S" "Sunrise/Sunset" sunrise-sunset :transient nil)]

   ["Fun"
    ("z" "Zone" zone :transient nil)]]

  [(casual-lib-quit-all)])

(transient-define-prefix cc/registers-tmenu ()
  ["Registers"
   ["Store"
    ("p" "Point…" point-to-register :transient nil)
    ("w" "Window Configuration…" window-configuration-to-register :transient nil)
    ("m" "Keyboard Macro…" kmacro-to-register :transient nil)
    ("j" "Jump…" jump-to-register :transient nil)]

   ["Store Text"
    ("c" "Region…" copy-to-register :if use-region-p :transient nil)
    ("r" "Rectangle…" copy-rectangle-to-register :if use-region-p :transient nil)
    ("a" "Append to Register…" append-to-register :if use-region-p :transient nil)
    ("P" "Prepend to Register…" prepend-to-register :if use-region-p :transient nil)
    ("i" "Insert Text…" insert-register :transient nil)]]

  [(casual-lib-quit-all)])

(transient-define-prefix cc/rectangle-tmenu ()
  ["Rectangle"
   ["Edit"
    ("k" "Kill" kill-rectangle :inapt-if-not use-region-p :transient nil)
    ("c" "Copy" copy-rectangle-as-kill :inapt-if-not use-region-p  :transient nil)
    ("y" "Yank" yank-rectangle :transient nil)
    ("d" "Delete" delete-rectangle :inapt-if-not use-region-p :transient nil)]

   ["Replace"
    ("s" "String" string-rectangle :inapt-if-not use-region-p  :transient nil)
    ("i" "String Insert" string-insert-rectangle :inapt-if-not use-region-p  :transient nil)
    ("o" "Open Insert" open-rectangle :inapt-if-not use-region-p :transient nil)]

   ["Misc"
    ("m" "Mark" rectangle-mark-mode :inapt-if-not use-region-p :transient nil)
    ("n" "Number" rectangle-number-lines :inapt-if-not use-region-p :transient nil)
    ("C" "Clear" clear-rectangle :inapt-if-not use-region-p :transient nil)
    ("D" "Delete Leading Spaces" delete-whitespace-rectangle :inapt-if-not use-region-p :transient nil)]]

  [(casual-lib-quit-all)])

(transient-define-prefix cc/transform-text-tmenu ()
  ["Transform"
   ("c" "Capitialize" capitalize-dwim :transient t)
   ("l" "Make Lower Case" downcase-dwim :transient t)
   ("u" "Make Upper Case" upcase-dwim :transient t)]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(provide 'cc-main-tmenu)
;;; cc-main-tmenu.el ends here
