;;; cc-org-agenda.el --- Org Agenda Configuration    -*- lexical-binding: t; -*-

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
(require 'org-agenda)
(require 'casual-lib)

(transient-define-prefix cc/org-agenda-tmenu ()
  "Org Agenda Transient"

  ["Org Agenda"
   ["Operations"
    :pad-keys t
    ("t" "Todo…" org-agenda-todo :transient t)
    ("C-s" "Schedule…" org-agenda-schedule :transient t)
    ("C-d" "Deadline…" org-agenda-deadline :transient t)
    (":" "Tags…" org-agenda-set-tags :transient t)
    ("+" "↑ Priority" org-agenda-priority-up :transient t)
    ("-" "↓ Priority" org-agenda-priority-down :transient t)
    ("C" "Clock›" cc/org-agenda-clock-tmenu)
    ("R" "Refile…" org-agenda-refile)
    ("z" "Add Note" org-agenda-add-note)
    ("A" "Archive…" org-agenda-archive-default-with-confirmation)]

   ["Mark"
    :pad-keys t
    ("m" "Mark" org-agenda-bulk-mark :transient t)
    ("%" "Mark Regexp…" org-agenda-bulk-mark-regexp :transient t)
    ("u" "Unmark" org-agenda-bulk-unmark :transient t)
    ("U" "Unmark" org-agenda-bulk-unmark-all :transient t)
    ("M-m" "Toggle" org-agenda-bulk-toggle :transient t)
    ("M-*" "Toggle all" org-agenda-bulk-toggle-all :transient t)
    ("B" "Bulk Action…" org-agenda-bulk-action :transient t)]

   ["Filter"
    ("/" "Filter…" org-agenda-filter :transient t)
    ("=" "Regexp…" org-agenda-filter-by-regexp :transient t)
    ("\\" "Tag…" org-agenda-filter-by-tag :transient t)
    ("^" "Headline…" org-agenda-filter-by-top-headline :transient t)
    ("<" "Category…" org-agenda-filter-by-category :transient t)
    ("_" "Effort…" org-agenda-filter-by-effort :transient t)
    ("|" "Remove all" org-agenda-filter-remove-all :transient t)]

   ["Navigation"
    :pad-keys t
    ("p" "↑" org-agenda-previous-line :transient t)
    ("n" "↓" org-agenda-next-line :transient t)
    ("P" "↑ ✲" org-agenda-previous-item :transient t)
    ("N" "↓ ✲" org-agenda-next-item :transient t)
    ("M-p" "↑ 📅" org-agenda-previous-date-line :transient t)
    ("M-n" "↓ 📅" org-agenda-next-date-line :transient t)
    ("j" "🚀 📅" org-agenda-goto-date)
    ("J" "🚀 ⏰" org-agenda-clock-goto :transient t)]]

  ["View"
   ["History"
    ("b" "←" org-agenda-earlier :transient t)
    ("f" "→" org-agenda-later :transient t)]

   ["Agenda"
    ("d" "Day" org-agenda-day-view :transient t)
    ("w" "Week" org-agenda-week-view :transient t)
    ("y" "Year" org-agenda-year-view :transient t)]

   ["Quick"
    ("s" "Save all" org-save-all-org-buffers)
    ("k" "Capture" org-capture)
    ("a" "Agenda" org-agenda)]

   ["Refresh"
    ("r" "Redo" org-agenda-redo :transient t)
    ("g" "Redo all" org-agenda-redo-all :transient t)]

   ["Utils"
    ("c" "📅" org-agenda-goto-calendar)
    ("S" "🌅" org-agenda-sunrise-sunset)
    ("M" "🌙" org-agenda-phases-of-moon)
    ("H" "Holidays" org-agenda-holidays)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("C-/" "Undo" org-agenda-undo)
          ("I" "ⓘ Info" org-info-find-node)
          (";" "⏱️" org-timer-set-timer)
          ("," "Settings›" cc/org-agenda-settings-tmenu :transient t)
          ("q" "Quit" org-agenda-quit)])


(transient-define-prefix cc/org-agenda-clock-tmenu ()
  ["Clock"
   ;;("I" "Clock In" org-clock-in) ; doesn't work needs org-element-at-point  which wants to be in Org Buffer, not the tmenu
   ("O" "Clock Out" org-clock-out)
   ("X" "Cancel" org-clock-cancel)
   ("m" "Modify" org-clock-modify-effort-estimate)]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])


(transient-define-prefix cc/org-agenda-settings-tmenu ()
  "Settings."
  ["Modes"
   ("l" "Log" org-agenda-log-mode
    :description (lambda () (casual-lib-checkbox-label org-agenda-show-log "Log"))
    :transient t)
   ("G" "Grid" org-agenda-toggle-time-grid
    :description (lambda () (casual-lib-checkbox-label org-agenda-use-time-grid "Grid"))
    :transient t)
   ("D" "Diary" org-agenda-toggle-diary
    :description (lambda () (casual-lib-checkbox-label org-agenda-include-diary "Diary"))
    :transient t)
   ("F" "Follow" org-agenda-follow-mode
    :description (lambda () (casual-lib-checkbox-label org-agenda-follow-mode "Follow"))
    :transient t)
   ("R" "Clock Report" org-agenda-clockreport-mode
    :description (lambda () (casual-lib-checkbox-label org-agenda-clockreport-mode "Clock Report"))
    :transient t)
   ("E" "Entry" org-agenda-entry-text-mode
    :description (lambda () (casual-lib-checkbox-label org-agenda-entry-text-mode "Entry"))
    :transient t)]

  [:class transient-row
          (casual-lib-quit-one)
          (casual-lib-quit-all)])

(keymap-set org-agenda-mode-map "C-o" #'cc/org-agenda-tmenu)

(provide 'cc-org-agenda)
;;; cc-org-agenda.el ends here
