;;; cc-ibuffer-mode.el --- ibuffer configuration     -*- lexical-binding: t; -*-

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

(require 'ibuffer)
(require 'hl-line)
(require 'mouse)
(require 'transient)

(add-hook 'ibuffer-mode-hook #'hl-line-mode)

;;(keymap-set ibuffer-mode-map "<mouse-1>" #'mouse-set-point)
(keymap-set ibuffer-mode-map "<double-mouse-1>" #'ibuffer-visit-buffer)
(keymap-set ibuffer-mode-map "M-<double-mouse-1>" #'ibuffer-visit-buffer-other-window)

(transient-define-prefix cc/ibuffer-tmenu ()
  [["Buffer"
    ("o" "Visit Other" ibuffer-visit-buffer-other-window)
    ("S" "Save" ibuffer-do-save)
    ("T" "Toggle Read-only" ibuffer-do-toggle-read-only)
    ("D" "Delete…" ibuffer-do-delete)
    ("M" "More Operations›" cc/ibuffer-operations-tmenu)]

   ["Display"
    ("s" "Sort By›" cc/ibuffer-sortby-tmenu :transient t)
    ("`" "Toggle Format" ibuffer-switch-format :transient t)
    ("g" "Refresh" ibuffer-update :transient t)]

   ["Mark"
    ("m" "Mark" ibuffer-mark-forward :transient t)
    ("t" "Type›" cc/ibuffer-mark-tmenu)
    ("r" "Regexp›" cc/ibuffer-mark-regexp-tmenu :transient t)
    ("u" "Unmark" ibuffer-unmark-forward :transient t)
    ("d" "To Delete" ibuffer-mark-for-delete :transient t)
    ("x" "Delete Marked (D)…" ibuffer-do-kill-on-deletion-marks)
    ("U" "Unmark All" ibuffer-unmark-all-marks :transient t)]

   ["Navigation"
    ("p" "Previous" ibuffer-backward-line :transient t)
    ("n" "Next" ibuffer-forward-line :transient t)
    ("j" "Jump…" ibuffer-jump-to-buffer :transient t)]]

  [["Filter"
    ("/" "Filter by…" ibuffer-filter-chosen-by-completion)
    ("|" "Disable" ibuffer-filter-disable)]

   ["Find, Replace"
    :pad-keys t
    ("O" "Occur…" ibuffer-do-occur)
    ("M-r" "Query Replace…" ibuffer-do-query-replace)
    ("C-M-r" "Query Replace Regexp…" ibuffer-do-query-replace-regexp)]
   ]
  [:class transient-row
          ("<return>" "Visit" ibuffer-visit-buffer)
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/ibuffer-operations-tmenu ()
  ["Buffer Operations"
   ;;("E" "Eval" ibuffer-do-eval)
   [("R" "Rename Uniquely…" ibuffer-do-rename-uniquely)
    ("!" "Shell…" ibuffer-do-shell-command-file)
    ("|" "Pipe to Shell…" ibuffer-do-shell-command-pipe)]

   [("=" "Diff Buffer/file" ibuffer-diff-with-file)
    ("B" "Copy Buffer Name" ibuffer-copy-buffername-as-kill)
    ("w" "Copy File Name" ibuffer-copy-filename-as-kill)]

   [("L" "Toggle Lock" ibuffer-do-toggle-lock)]]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(transient-define-prefix cc/ibuffer-sortby-tmenu ()
  ["Sort By"
   [("v" "Recency" ibuffer-do-sort-by-recency) ;; changed from "v"
    ("a" "Buffer Name" ibuffer-do-sort-by-alphabetic)
    ("f" "Filename/Process" ibuffer-do-sort-by-filename/process)]

   [("m" "Major Mode" ibuffer-do-sort-by-major-mode)
    ("s" "Size" ibuffer-do-sort-by-size)]

   [("i" "Invert" ibuffer-invert-sorting)]]

  [:class transient-row
          ("," "Cycle Sort" ibuffer-toggle-sorting-mode)
          ("q" "Dismiss" ignore :transient transient--do-exit)])


(transient-define-prefix cc/ibuffer-mark-tmenu ()
  ["Mark By"
   [("m" "Mode" ibuffer-mark-by-mode)
    ("d" "Dired" ibuffer-mark-dired-buffers)
    ("h" "Help" ibuffer-mark-help-buffers)]

   [("*" "Modified" ibuffer-mark-modified-buffers)
    ("r" "Read-only" ibuffer-mark-read-only-buffers)
    ("u" "Unsaved" ibuffer-mark-unsaved-buffers)]

   [("D" "Dissociated" ibuffer-mark-dissociated-buffers)
    ("s" "*Special*" ibuffer-mark-special-buffers)
    ("z" "Compressed" ibuffer-mark-compressed-file-buffers)]]

  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])


(transient-define-prefix cc/ibuffer-mark-regexp-tmenu ()
  ["Mark Regexp"
   ("m" "Mode" ibuffer-mark-by-mode-regexp)
   ("n" "Buffer Name" ibuffer-mark-by-name-regexp)
   ("f" "File name" ibuffer-mark-by-file-name-regexp)
   ("c" "Content" ibuffer-mark-by-content-regexp)]
  [:class transient-row
          ("q" "Dismiss" ignore :transient transient--do-exit)])

(keymap-set ibuffer-mode-map "s" #'cc/ibuffer-sortby-tmenu)
(keymap-set ibuffer-mode-map "C-o" #'cc/ibuffer-tmenu)

(provide 'cc-ibuffer-mode)
;;; cc-ibuffer-mode.el ends here
