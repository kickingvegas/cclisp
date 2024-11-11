;;; cc-calendar-mode-constants.el --- Calendar Mode Constants   -*- lexical-binding: t; -*-

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

(defconst casual-calendar--navigation-group
  ["Navigation"
   ["Day"
    ("b" "Behind" calendar-backward-day :transient t)
    ("f" "Ahead" calendar-forward-day :transient t)
    ("." "Today" calendar-goto-today :transient t)
    ("g" "Goto…" calendar-goto-date :transient t)]
   ["Week"
    ("p" "Behind" calendar-backward-week :transient t)
    ("n" "Ahead" calendar-forward-week :transient t)
    ("a" "Beginning" calendar-beginning-of-week :transient t)
    ("e" "End" calendar-end-of-week :transient t)
    ("w" "Goto…" calendar-iso-goto-week :transient t)]

   ["Month"
    :pad-keys t
    ("{" "Behind" calendar-backward-month :transient t)
    ("}" "Ahead" calendar-forward-month :transient t)
    ("M-a" "Beginning" calendar-beginning-of-month :transient t)
    ("M-e" "End" calendar-end-of-month :transient t)
    ("o" "Goto…" calendar-other-month :transient t)]

   ["Year"
    :pad-keys t
    ("M-[" "Behind" calendar-backward-year :transient t)
    ("M-]" "Ahead" calendar-forward-year :transient t)
    ("[" "Beginning" calendar-beginning-of-year :transient t)
    ("]" "End" calendar-end-of-year :transient t)]

   ["Scroll"
    :pad-keys t
    ("<" "Behind" calendar-scroll-right :transient t)
    (">" "Ahead" calendar-scroll-left :transient t)
    ("-" "-3 months" calendar-scroll-right-three-months :transient t)
    ("+" "+3 months" calendar-scroll-left-three-months :transient t)
    ("C-l" "Redraw" calendar-redraw :transient t)]]
  "Transient navigation group for calendar.")



(defconst casual-calendar--menu-navigation-group
  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   (casual-lib-quit-all)]
  "Transient menu navigation group for calendar.")

(provide 'cc-calendar-mode-constants)
;;; cc-calendar-mode.el ends here
