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

(defun cc/context-menu-addons (menu click)
  "Context menu customization for Charles Choi.
MENU - menu
CLICK - event"
  (save-excursion
    (mouse-set-point click)

    (easy-menu-add-item menu nil ["Journal"
                                  status-report
                                  :help "Go to current day journal"])

    (easy-menu-add-item menu nil ["Agenda - All TODOs"
                                  (lambda () (interactive)(org-agenda nil "n"))
                                  :help "Show Org agenda with all TODO tasks."])

    (cc/context-menu-item-separator menu buffer-navigation-separator)

    (easy-menu-add-item menu nil ["List All Buffers"
                                  ibuffer
                                  :help "List all buffers"])

    (easy-menu-add-item menu nil ["Previous Buffer"
                                  previous-buffer
                                  :help "Go to previous buffer"])

    (easy-menu-add-item menu nil ["Next Buffer"
                                  next-buffer
                                  :help "Go to next buffer"])

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
from current buffer"])))

    (easy-menu-add-item menu nil "--")

    (cc/context-menu-item-separator menu capture-flow-separator)
    (easy-menu-add-item menu nil
                        ["New Workflow…"
                         org-capture
                         :help "Create new task or workflow via org-capture"])

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
                         :help "Open file in Dired"])

    (easy-menu-add-item menu nil casual-dired-sort-menu)
    (when (derived-mode-p 'dired-mode)
      (easy-menu-add-item menu nil
                          ["Duplicate"
                           cc/dired-duplicate-file
                           :label (concat "Duplicate"
                                          " “"
                                          (file-name-base (dired-get-filename))
                                          "."
                                          (file-name-extension (dired-get-filename))
                                          "”")
                           :help "Duplicate selected item"])
      ;; (easy-menu-add-item menu nil
      ;;                     ["Image Info"
      ;;                      cc/kill-image-info
      ;;                      :label (concat
      ;;                              "Info: "
      ;;                              (casual-dired--identify-image
      ;;                               (dired-get-filename)))
      ;;                      :visible (casual-dired-image-file-p)])
      )

    (when (use-region-p)
      (cc/context-menu-item-separator menu dictionary-operations-separator)
      (easy-menu-add-item menu nil ["Look Up"
                                    osx-dictionary-search-word-at-point
                                    :label (cc/context-menu-last-word-in-region "Look Up")
                                    :help "Look up selected region in macOS dictionary"]))

    (cc/context-menu-item-separator menu find-operations-separator)
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
containing a match for regex"]))

    ;;(easy-menu-add-item menu nil cc/find-menu)

    (keymap-set-after menu
      "<vc-separator>"
      '(menu-item "--"
                  :visible (vc-responsible-backend default-directory t))
      'Find\ and/or\ Replace)

    (if (vc-responsible-backend default-directory t)
        (easy-menu-add-item
         menu nil
         ["Magit Status"
          magit-status
          :help "Show the status of the current Git repository in a buffer"]))

    (easy-menu-add-item
     menu nil
     ["Ediff revision…"
      cc/ediff-revision-from-menu
      :visible (and (bound-and-true-p buffer-file-name)
                    (vc-registered (buffer-file-name)))
      :help "Ediff this file with revision"])

    (when (use-region-p)
      (cc/context-menu-item-separator menu transform-text-separator)
      (easy-menu-add-item menu nil cc/transform-text-menu)

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
        (easy-menu-add-item menu nil cc/emphasize-menu))))

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
                           org-insert-last-stored-link
                           :enable (cc/org-stored-links-p)
                           :help "Insert the last link stored in org-stored-links"]))

     ((derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu markdown-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Reveal Markup"
                           markdown-toggle-markup-hiding
                           :help "Toggle the display or hiding of markup"])))

    (when (org-at-table-p)
      (cc/context-menu-item-separator menu org-table-separator)
      (easy-menu-add-item menu nil
                          ["Table Cell Info"
                           cc/mouse-copy-org-table-range-dwim
                           :label (cc/org-table-range-dwim)
                           :help "Table field/cell information"])
      (easy-menu-add-item menu nil cc/insert-org-plot-menu)
      (easy-menu-add-item menu nil ["Run gnuplot"
                                    org-plot/gnuplot
                                    :help "Plot table using gnuplot"]))

    (when (use-region-p)
      (cc/context-menu-item-separator menu region-operations-separator)
      (easy-menu-add-item menu nil cc/region-operations-menu))

    (cc/context-menu-item-separator menu world-clock-separator)
    (easy-menu-add-item menu nil
                        ["Calendar"
                         calendar
                         :help "Display a three-month Gregorian calendar"])
    (easy-menu-add-item menu nil
                        ["World Clock"
                         world-clock
                         :help "Display times from around the world"])

    (cc/context-menu-item-separator menu count-words-separator)

    (when (derived-mode-p 'text-mode)
      (if (use-region-p)
          (easy-menu-add-item menu nil ["Count Words in Region"
                                        count-words
                                        :help "Count words in region"])

        (easy-menu-add-item menu nil ["Count Words in Buffer"
                                      count-words
                                      :help "Count words in buffer"])))

    (easy-menu-add-item menu nil cc/wgrep-menu)

    menu))

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


(add-hook 'context-menu-functions #'cc/context-menu-addons)

(provide 'cc-context-menu)
;;; cc-context-menu.el ends here
