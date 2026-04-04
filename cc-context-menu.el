;;; cc-context-menu.el --- Context Menu Customization -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026  Charles Choi

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
(require 'easymenu)
(require 'mouse)
(require 'org)
(require 'org-agenda)
(require 'reveal-in-folder)
(require 'cclisp)
(require 'cc-region-operations-menu)
(require 'reveal-in-folder)
(require 'osx-dictionary)
(require 'ox-slack)
(require 'compile)
(require 'casual-agenda)
(require 'anju)

(easy-menu-define cc/context-menu-journal-menu nil
  "Key map for Org copy sub-menu."
  '("Journal"

    ["Journal"
     status-report
     :help "Go to current day journal"]

    ["Agenda - All TODOs"
     (lambda () (interactive)(org-agenda nil "n"))
     :help "Show Org agenda with all TODO tasks"]

    ["Workflow…"
     org-capture
     :help "Capture content via Org"]

    ["Scratch"
     scratch-buffer
     :help "Switch to the *scratch* buffer."]))

(defun cc/context-menu-journal (menu click)
  "Context menu hook function for journal commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."

  (when (and (not (anju-at-org-table-p))
             (not (use-region-p)))

    (save-excursion
      (mouse-set-point click)
      (anju-context-menu-item-separator menu journal-separator)

      (easy-menu-add-item menu nil cc/context-menu-journal-menu)

      (easy-menu-add-item menu nil ["Add Note"
                                    (lambda () (interactive)(org-capture nil "j"))
                                    :help "Add journal note"])))
  menu)


(defun cc/context-menu-region (menu click)
  "Context menu hook function for region commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."
  (if (use-region-p)
      (save-excursion
        (mouse-set-point click)
        (easy-menu-add-item menu nil cc/region-operations-menu)))
  menu)

(defun cc/context-menu-dired (menu click)
  "Context menu hook function for Dired commands.

Adds Finder/File Manager to Dired.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."
  (when (derived-mode-p 'dired-mode)
    (save-excursion
      (mouse-set-point click)
      (easy-menu-add-item menu nil
                          ["Open in File Manager"
                           reveal-in-folder-at-point
                           :label (format
                                   "📁 Open in %s"
                                   (if (eq (window-system) 'ns)
                                       "Finder"
                                     "File Manager"))
                           :help "Open file (buffer) in Finder"])))
  menu)


(defun cc/context-menu-open-in (menu click)
  "Context menu hook function for open-in commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."
  (when (and (not (use-region-p))
             (not (anju-at-org-table-p))
             (not (derived-mode-p 'dired-mode)))
    (save-excursion
      (mouse-set-point click)
      (easy-menu-add-item menu nil
                          ["📁 Open in Finder"
                           reveal-in-folder-this-buffer
                           :visible (buffer-file-name)
                           :help "Open file (buffer) in Finder"])))
  menu)

(defun cc/context-menu-dictionary (menu click)
  "Context menu hook function for dictionary commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."
  (when (use-region-p)
    (save-excursion
      (mouse-set-point click)
      (easy-menu-add-item menu nil ["Look Up"
                                    osx-dictionary-search-word-at-point
                                    :label (anju-menu-label "Look Up")
                                    :help "Look up selected region in macOS dictionary"])))
  menu)


(easy-menu-define cc/context-menu-org-copy-as-menu nil
  "Key map for Org copy sub-menu."
  '("Copy as…"
    :visible (and (derived-mode-p 'org-mode) (use-region-p))

    ["Markdown"
     mb/org-copy-region-as-markdown
     :help "Copy region as Markdown"]

    ["Slack"
     org-slack-export-to-clipboard-as-slack
     :visible (package-installed-p 'ox-slack)
     :help "Copy as Slack to clipboard"]

    ["RTF"
     dm/copy-as-rtf
     :help "Copy as RTF to clipboard"]))

(defun cc/context-menu-region-extension (menu click)
  "Region menu using MENU and CLICK."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (mouse-set-point click)
      (easy-menu-add-item menu nil cc/context-menu-org-copy-as-menu
                          "Paste")))
  menu)

(easy-menu-define cc/context-menu-org-agenda-view-menu nil
  "Key map for Org agenda view sub-menu."
  '("View"
    :visible (and (derived-mode-p 'org-agenda-mode) (casual-agenda-type-agendap))

    ["← Earlier"
     org-agenda-earlier
     :help "Agenda view earlier"]

    ["→ Later"
     org-agenda-later
     :help "Agenda view later"]

    ["Day"
     org-agenda-day-view
     :help "Agenda day view"]

    ["Week"
     org-agenda-week-view
     :help "Agenda week view"]

    ["Fortnight"
     org-agenda-fortnight-view
     :help "Agenda fortnight view"]

    ["Month"
     org-agenda-month-view
     :help "Agenda month view"]

    ["Year"
     org-agenda-year-view
     :help "Agenda year view"]))


(defun cc/context-menu-org-agenda (menu click)
  "Context menu hook function for Org agenda commands.

- MENU: menu
- CLICK: event

This function is intended to be hooked into `context-menu-functions'."

  (when (derived-mode-p 'org-agenda-mode)
    (mouse-set-point click)
    (save-excursion
      (when (casual-agenda-headlinep)
        (easy-menu-add-item menu nil ["Clock In"
                                      casual-agenda-clock-in
                                      :label (anju-middle-truncate (org-agenda-with-point-at-orig-entry nil
                                                                     (org-element-property :title (org-element-at-point)))
                                                                   "Clock In")
                                      :visible (not (org-clocking-p))
                                      :help "Clock in"])

        (easy-menu-add-item menu nil ["Clock Out"
                                      casual-agenda-clock-out
                                      :visible (org-clocking-p)
                                      :help "Clock out"])

        (easy-menu-add-item menu nil ["Schedule…"
                                      org-agenda-schedule
                                      :help "Schedule headline"])

        (easy-menu-add-item menu nil ["Deadline…"
                                      org-agenda-deadline
                                      :help "Deadline headline"])

        (easy-menu-add-item menu nil ["↑ Priority"
                                      org-agenda-priority-up
                                      :help "Up priority"])

        (easy-menu-add-item menu nil ["↓ Priority"
                                      org-agenda-priority-down
                                      :help "Down priority"])

        (easy-menu-add-item menu nil ["Todo…"
                                      org-agenda-todo
                                      :help "Set Todo"])

        (easy-menu-add-item menu nil ["Tags…"
                                      org-agenda-set-tags
                                      :help "Set Tags"])

        (easy-menu-add-item menu nil ["Note…"
                                      org-agenda-add-note
                                      :help "Add note"]))

      (easy-menu-add-item menu nil ["Now"
                                    casual-agenda-goto-now
                                    :help "Goto now"])

      (easy-menu-add-item menu nil cc/context-menu-org-agenda-view-menu)

      (easy-menu-add-item menu nil ["Refresh"
                                    org-agenda-redo-all
                                    :help "Redo all"])))
  menu)


(provide 'cc-context-menu)
;;; cc-context-menu.el ends here
