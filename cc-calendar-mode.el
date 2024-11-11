;;; cc-calendar-mode.el --- Calendar Configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: calendar

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
(require 'calendar)
(require 'casual-lib)
(require 'cc-calendar-mode-constants)

;; '(org-agenda-include-diary t)
;; (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
;; (add-hook 'diary-list-entries-hook #'diary-sort-entries t)

;; (add-hook 'diary-nongregorian-listing-hook 'diary-chinese-list-entries)
;; (add-hook 'diary-nongregorian-marking-hook 'diary-chinese-mark-entries)
;; (add-hook 'calendar-mode-hook #'diary-mark-entries)

;; '(calendar-latitude 37.7643)
;; '(calendar-location-name '(format "%s, San Francisco" "Inner Sunset"))
;; '(calendar-longitude -122.4753)
;; '(calendar-mark-holidays-flag t)
;; '(calendar-move-hook '(calendar-update-mode-line))

(transient-define-prefix casual-calendar-tmenu ()
  "calendar menu"
  casual-calendar--navigation-group

  [["Conversions"
    ("c" "Conversions›" casual-calendar-conversions-tmenu)
    ("A" "All other dates" calendar-print-other-dates :transient t)]

   ["Holidays"
    ("H" "Holidays in span" calendar-list-holidays :transient t)
    ("h" "Holidays at point" calendar-cursor-holidays :transient t)
    ("x" "Mark Holidays" calendar-mark-holidays :transient t)
    ("u" "Unmark" calendar-unmark :transient t)]

   ["Misc"
    ("O" "Org Agenda" org-calendar-goto-agenda)
    ("d" "Diary" diary-view-entries :transient t)
    ("s" "All Diary" diary-show-all-entries :transient t)
    ("D" "Diary & Goto›" casual-calendar-diary-and-goto-tmenu)]

   ["Almanac"
    :pad-keys t
    ("M" "Lunar Phases" calendar-lunar-phases :transient t)
    ("S" "Sunrise/Sunset" calendar-sunrise-sunset :transient t)
    ("M-m" "Sunrise/Sunset Month" calendar-sunrise-sunset-month :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   ("I" "ⓘ Info" (lambda ()
                   (interactive)
                   (calendar-exit)
                   (calendar-goto-info-node)))
   ("q" "Quit" calendar-exit)])

(transient-define-prefix casual-calendar-diary-and-goto-tmenu ()
  "goto menu"

  ["Diary and Goto"
   ["Diary Insert"
    ("e" "Entry" diary-insert-entry :transient t)
    ("w" "Weekly" diary-insert-weekly-entry :transient t)
    ("m" "Monthly" diary-insert-monthly-entry :transient t)
    ("y" "Yearly" diary-insert-yearly-entry :transient t)
    ("a" "Anniversary" diary-insert-anniversary-entry :transient t)]

   ["Goto"
    ("g" "Date…" calendar-goto-date :transient t)
    ("i" "ISO Date…" calendar-iso-goto-date :transient t)
    ("d" "Day of Year…" calendar-goto-day-of-year :transient t)]]

  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-conversions-tmenu ()
  "conversions menu"

  ["Calendars"
   [("a" "Astronomical›" casual-calendar-astro-tmenu)
    ("b" "Bahai›" casual-calendar-bahai-tmenu)
    ("c" "Coptic›" casual-calendar-coptic-tmenu)]

   [("e" "Ethiopic›" casual-calendar-ethiopic-tmenu)
    ("f" "French Revolutionary›" casual-calendar-french-tmenu)
    ("h" "Hebrew›" casual-calendar-hebrew-tmenu)]

   [("i" "Islamic›" casual-calendar-islamic-tmenu)
    ("j" "Julian›" casual-calendar-julian-tmenu)
    ("l" "Lunar (Chinese)›" casual-calendar-lunar-tmenu)]

   [("m" "Mayan›" casual-calendar-mayan-tmenu)
    ("p" "Persian›" casual-calendar-persian-tmenu)]]

  ["All"
   ("A" "All other dates" calendar-print-other-dates :transient t)]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   ("I" "ⓘ Info" (lambda ()
                   (interactive)
                   (calendar-exit)
                   (info "(emacs) Other Calendars")))
   (casual-lib-quit-all)])

(transient-define-prefix casual-calendar-lunar-tmenu ()
  "Lunar calendar menu"
  ["Lunar (Chinese) Calendar"
   ["Date"
    ("c" "Date at cursor" calendar-chinese-print-date :transient t)
    ("G" "Goto…" calendar-chinese-goto-date :transient t)]

   ["Diary Insert"
    ("i" "Point" diary-chinese-insert-entry)
    ("m" "Monthly" diary-chinese-insert-monthly-entry)
    ("y" "Year" diary-chinese-insert-yearly-entry)
    ("A" "Anniversary" diary-chinese-insert-anniversary-entry)]

   ["Diary View"
    ("d" "View" diary-view-entries :transient t)
    ("s" "Show all" diary-show-all-entries)]]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-astro-tmenu ()
  "Astronomical calendar menu"
  ["Astronomical Calendar"
   ("c" "Day number at cursor" calendar-astro-print-day-number :transient t)
   ("G" "Goto…" calendar-astro-goto-day-number :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-islamic-tmenu ()
  "Islamic calendar menu"
  ["Islamic Calendar"
   ("c" "Date at cursor" calendar-islamic-print-date :transient t)
   ("G" "Goto…" calendar-islamic-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-hebrew-tmenu ()
  "Hebrew calendar menu"
  ["Hebrew Calendar"
   ("c" "Date at cursor" calendar-hebrew-print-date :transient t)
   ("G" "Goto…" calendar-hebrew-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-bahai-tmenu ()
  "Bahai calendar menu"

  ["Bahai Calendar"
   ("c" "Date at cursor" calendar-bahai-print-date :transient t)
   ("G" "Goto…" calendar-bahai-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-ethiopic-tmenu ()
  "Ethiopic calendar menu"
  ["Ethiopic Calendar"
   ("c" "Date at cursor" calendar-ethiopic-print-date :transient t)
   ("G" "Goto…" calendar-ethiopic-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-french-tmenu ()
  "French revolutionary calendar menu"
  ["French Revolutionary Calendar"
   ("c" "Date at cursor" calendar-french-print-date :transient t)
   ("G" "Goto…" calendar-french-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-julian-tmenu ()
  "Julian calendar menu"
  ["Julian Calendar"
   ("c" "Date at cursor" calendar-julian-print-date :transient t)
   ("G" "Goto…" calendar-julian-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-coptic-tmenu ()
  "Coptic calendar menu"
  ["Coptic Calendar"
   ("c" "Date at cursor" calendar-coptic-print-date :transient t)
   ("G" "Goto…" calendar-coptic-goto-date :transient t)]
  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-persian-tmenu ()
  "Persian calendar menu"
  ["Persian Calendar"
   ("c" "Date at cursor" calendar-persian-print-date :transient t)
   ("G" "Goto…" calendar-persian-goto-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(transient-define-prefix casual-calendar-mayan-tmenu ()
  "Mayan calendar menu"
  [" Calendar"
   ("c" "Date at cursor" calendar-mayan-print-date :transient t)
   ("G" "Goto long count…" calendar-mayan-goto-long-count-date :transient t)]

  casual-calendar--navigation-group
  casual-calendar--menu-navigation-group)

(keymap-set calendar-mode-map "C-o" #'casual-calendar-tmenu)
; (add-hook 'calendar-mode-hook #'calendar-mark-holidays)
(add-hook 'calendar-mode-hook #'diary-mark-entries)

(provide 'cc-calendar-mode)
;;; cc-calendar-mode.el ends here
