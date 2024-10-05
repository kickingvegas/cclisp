;;; cc-context-menu.el --- Context Menu Customization -*- lexical-binding: t -*-

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
(require 'easymenu)
(require 'mouse)
(require 'org)
(require 'cclisp)
(require 'cc-context-menu-macros)
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'cc-region-operations-menu)
(require 'cc-insert-org-plot)
(require 'cc-find-menu)
(require 'cc-edit-text-menu)
(require 'cc-wgrep-mode)
(require 'cc-dired-mode)
(require 'casual-dired)

(defun cc/context-menu-addon-items (menu click)
  "Charles Choi context menu hook function using MENU and CLICK event.

MENU - menu to be configured.
CLICK - event"
  (save-excursion
    (mouse-set-point click)
    (cc/context-menu-journal-items menu)
    (cc/context-menu-org-table-items menu (not (org-at-table-p)))
    (cc/context-menu-buffers-items menu)
    (cc/context-menu-narrow-items menu)
    (cc/context-menu-workflow-items menu)
    (cc/context-menu-open-in-items menu)
    (cc/context-menu-dired-items menu (not (derived-mode-p 'dired-mode)))
    (cc/context-menu-dictionary-items menu (not (use-region-p)))
    (cc/context-menu-occur-items menu)
    (cc/context-menu-vc-items menu (not (vc-responsible-backend default-directory t)))
    (cc/context-menu-region-actions-items menu (not (use-region-p)))
    (cc/context-menu-reveal-markup-items menu)
    (cc/context-menu-timekeeping-items menu)
    (cc/context-menu-word-count-items menu (not (derived-mode-p 'text-mode)))
    (easy-menu-add-item menu nil cc/wgrep-menu)
    menu))

(defun cc/context-menu-word-count-items (menu &optional inapt)
  "Menu items to populate MENU for word count section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu count-words-separator)
    (if (use-region-p)
          (easy-menu-add-item menu nil ["Count Words in Region"
                                        count-words
                                        :help "Count words in region"])

        (easy-menu-add-item menu nil ["Count Words in Buffer"
                                      count-words
                                      :help "Count words in buffer"]))))1

(defun cc/context-menu-reveal-markup-items (menu &optional inapt)
  "Menu items to populate MENU for reveal markup section if INAPT nil."
  (when (not inapt)
    (cond
     ((derived-mode-p 'org-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Reveal Markup"
                           visible-mode
                           :help "Toggle making all invisible text \
temporarily visible (Visible mode)"])

      (easy-menu-add-item menu nil
                          ["Paste Last Org Link"
                           org-insert-last-stored-linkyy
                           :enable (cc/org-stored-links-p)
                           :help "Insert the last link stored in org-stored-links"]))

     ((derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu markdown-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Reveal Markup"
                           markdown-toggle-markup-hiding
                           :help "Toggle the display or hiding of markup"])))))

(defun cc/context-menu-vc-items (menu &optional inapt)
  "Menu items to populate MENU for version control section if INAPT nil."
  (when (not inapt)
    (keymap-set-after menu
      "<vc-separator>"
      '(menu-item "--"
                  :visible (vc-responsible-backend default-directory t))
      'Find\ and/or\ Replace)

    (easy-menu-add-item
     menu nil
     ["Magit Status"
      magit-status
      :help "Show the status of the current Git repository in a buffer"])

    (easy-menu-add-item
     menu nil
     ["Ediff revision…"
      cc/ediff-revision-from-menu
      :visible (and (bound-and-true-p buffer-file-name)
                    (vc-registered (buffer-file-name)))
      :help "Ediff this file with revision"])))

(defun cc/context-menu-region-actions-items (menu &optional inapt)
  "Menu items to populate MENU for region actions section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu transform-text-separator)
    (easy-menu-add-item menu nil cc/transform-text-menu)
    (easy-menu-add-item menu nil cc/region-operations-menu)
    (cond
     ((derived-mode-p 'prog-mode)
      (easy-menu-add-item menu nil
                          ["Toggle Comment"
                           comment-dwim
                           :help "Toggle comment on selected region"]))

     ((derived-mode-p 'org-mode)
      (easy-menu-add-item menu nil cc/emphasize-menu)
      (easy-menu-add-item menu nil ["Copy as Slack"
                                    org-slack-export-to-clipboard-as-slack
                                    :help "Copy as Slack to clipboard"])
      (easy-menu-add-item menu nil ["Copy as Slack"
                                    org-slack-export-to-clipboard-as-slack
                                    :help "Copy as Slack to clipboard"])
      (easy-menu-add-item menu nil ["Copy as RTF"
                                    dm/copy-as-rtf
                                    :help "Copy as RTF to clipboard"]))

     ((derived-mode-p 'markdown-mode)
      (easy-menu-add-item menu nil cc/emphasize-menu)))))

(defun cc/context-menu-timekeeping-items (menu &optional inapt)
  "Menu items to populate MENU for timekeeping section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu world-clock-separator)
    (easy-menu-add-item menu nil
                        ["Calendar"
                         calendar
                         :help "Display a three-month Gregorian calendar"])
    (easy-menu-add-item menu nil
                        ["World Clock"
                         world-clock
                         :help "Display times from around the world"])))

(defun cc/context-menu-journal-items (menu &optional inapt)
  "Menu items to populate MENU for journal section if INAPT nil."
  (when (not inapt)
    (easy-menu-add-item menu nil ["Journal"
                                  status-report
                                  :help "Go to current day journal"])

    (easy-menu-add-item menu nil ["Agenda - All TODOs"
                                  (lambda () (interactive)(org-agenda nil "n"))
                                  :help "Show Org agenda with all TODO tasks."])))

(defun cc/context-menu-dictionary-items (menu &optional inapt)
  "Menu items to populate MENU for <replace> section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu dictionary-operations-separator)
    (easy-menu-add-item menu nil ["Look Up"
                                  osx-dictionary-search-word-at-point
                                  :label (cc/context-menu-last-word-in-region "Look Up")
                                  :help "Look up selected region in macOS dictionary"])))

(defun cc/context-menu-occur-items (menu &optional inapt)
  "Menu items to populate MENU for occur section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu occur-separator)
    ;;(easy-menu-add-item menu nil cc/find-menu)
    (if (use-region-p)
        (easy-menu-add-item menu nil
                            ["Find word in buffer (occur)"
                             ;;occur-word-at-mouse
                             occur-symbol-at-mouse
                             :visible (not buffer-read-only)
                             :label (cc/context-menu-last-word-in-region
                                     "Occur")
                             :help "Show all lines in the current buffer containing \
a match for selected word"])
      (easy-menu-add-item menu nil
                          ["Occur…"
                           occur
                           :visible (not buffer-read-only)
                           :help "Show all lines in the current buffer \
containing a match for regex"]))))

(defun cc/context-menu-dired-items (menu &optional inapt)
  "Menu items to populate MENU for Dired section if INAPT nil."
  (when (not inapt)
    (easy-menu-add-item menu nil casual-dired-sort-menu)
    (easy-menu-add-item menu nil
                        ["Duplicate"
                         cc/dired-duplicate-file
                         :label (concat "Duplicate"
                                        " “"
                                        (file-name-base (dired-get-filename))
                                        "."
                                        (file-name-extension (dired-get-filename))
                                        "”")
                         :help "Duplicate selected item"])))


(defun cc/context-menu-workflow-items (menu &optional inapt)
  "Menu items to populate MENU for workflow section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu capture-flow-separator)
    (easy-menu-add-item menu nil
                        ["New Workflow…"
                         org-capture
                         :help "Create new task or workflow via org-capture"])))

(defun cc/context-menu-buffers-items (menu &optional inapt)
  "Menu items to populate MENU for buffers section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu buffer-navigation-separator)

    (easy-menu-add-item menu nil ["List All Buffers"
                                  ibuffer
                                  :help "List all buffers"])

    (easy-menu-add-item menu nil ["Previous Buffer"
                                  previous-buffer
                                  :help "Go to previous buffer"])

    (easy-menu-add-item menu nil ["Next Buffer"
                                  next-buffer
                                  :help "Go to next buffer"])))


(defun cc/context-menu-open-in-items (menu &optional inapt)
  "Menu items to populate MENU for open in section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu open-in-separator)

    (easy-menu-add-item menu nil
                        ["Open in Finder"
                         reveal-in-folder-this-buffer
                         :visible (or (buffer-file-name) (derived-mode-p 'dired-mode))
                         :help "Open file (buffer) in Finder"])

    (easy-menu-add-item menu nil
                        ["Open in Dired"
                         dired-jump-other-window
                         :visible (and (buffer-file-name) (not (derived-mode-p 'dired-mode)))
                         :help "Open file in Dired"])))


(defun cc/context-menu-narrow-items (menu &optional inapt)
  "Menu items to populate MENU for narrow section if INAPT nil."
  (when (not inapt)
    (when buffer-file-name
      (cond ((use-region-p)
             (cc/context-menu-item-separator menu narrow-separator)
             (easy-menu-add-item menu nil
                                 ["Narrow Region" narrow-to-region
                                  :label (cc/context-menu-label "Narrow Region")
                                  :help "Restrict editing in this buffer \
to the current region"]))

            ((and (not (buffer-narrowed-p)) (derived-mode-p 'prog-mode))
             (cc/context-menu-item-separator menu narrow-separator)
             (easy-menu-add-item menu nil
                                 ["Narrow to defun" narrow-to-defun
                                  :help "Restrict editing in this buffer \
to the current defun"]))

            ((and (not (buffer-narrowed-p)) (derived-mode-p 'org-mode))
             (cc/context-menu-item-separator menu narrow-separator)
             (easy-menu-add-item menu nil
                                 ["Narrow to subtree" org-narrow-to-subtree
                                  :help "Restrict editing in this buffer \
to the current subtree"]))


            ((and (not (buffer-narrowed-p)) (derived-mode-p 'markdown-mode))
             (cc/context-menu-item-separator menu narrow-separator)
             (easy-menu-add-item menu nil
                                 ["Narrow to subtree" markdown-narrow-to-subtree
                                  :help "Restrict editing in this buffer \
to the current subtree"])))

      (when (buffer-narrowed-p)
        (cc/context-menu-item-separator menu widen-separator)
        (easy-menu-add-item menu nil
                            ["Widen buffer" widen
                             :help "Remove narrowing restrictions \
from current buffer"])))))

(defun cc/context-menu-org-table-items (menu &optional inapt)
  "Menu items to populate MENU for Org table section if INAPT nil."
  (when (not inapt)
    (cc/context-menu-item-separator menu org-table-sqeparator)
    (easy-menu-add-item menu nil
                        ["Table Cell Info"
                         cc/mouse-copy-org-table-reference-dwim
                         :label (cc/org-table-reference-dwim)
                         :help "Copy Org table reference (field or range) into kill ring via mouse"])
    (easy-menu-add-item menu nil
                        ["Show Coordinates"
                         org-table-toggle-coordinate-overlays
                         :style toggle
                         :selected org-table-coordinate-overlays
                         :help "Toggle the display of row/column numbers in tables"])
    (easy-menu-add-item menu nil cc/insert-org-plot-menu)
    (easy-menu-add-item menu nil ["Run gnuplot"
                                  org-plot/gnuplot
                                  :help "Plot table using gnuplot"])))

(add-hook 'context-menu-functions #'cc/context-menu-addon-items)

;; (defvar cchoi-mouse-file nil "some thing")

;; (defun cc/track-mouse (event)
;;   (interactive "e")
;;   (if (mouse-posn-property (event-start event) 'dired-filename)
;;     (let (window pos file)
;;       (save-excursion
;;         (setq window (posn-window (event-end event))
;;            pos (posn-point (event-end event)))
;;         (if (not (windowp window))
;;          (error "No file chosen"))
;;         (set-buffer (window-buffer window))
;;         (goto-char pos)
;;         (setq file (dired-get-filename)))
;;       (setq cchoi-mouse-file file)))
;;   (setq cchoi-mouse-file nil))


;; (defun cc/kill-image-info (e)
;;   "Show file type.
;; E - event"
;;   (interactive "e")
;;   (ignore e)
;;   (dired-show-file-type))

(provide 'cc-context-menu)
;;; cc-context-menu.el ends here
