;;; cc-context-menu.el --- Context Menu Customization -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025  Charles Choi

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
(require 'casual-ediff-utils)
(require 'casual-org)
(require 'yank-media)


(defun cc/yank-media-p ()
  "Predicate if media (images, HTML and the like) is in the clipboard.

This is built using the implementation of `yank-media'."
  (interactive)
  (unless yank-media--registered-handlers
    (user-error "The `%s' mode hasn't registered any handlers" major-mode))
  (let ((all-types nil))
    (pcase-dolist (`(,handled-type . ,handler)
                   yank-media--registered-handlers)
      (dolist (type (yank-media--find-matching-media handled-type))
        (push (cons type handler) all-types)))
    (if all-types t nil)))

(defcustom cc/context-menu-functions-and-predicates
  '((cc/context-menu-dired-items . (lambda () (or
                                          (org-at-table-p)
                                          (not (derived-mode-p 'dired-mode)))))
    (cc/context-menu-journal-items . cc/org-at-table-or-use-region-p)
    (cc/context-menu-org-table-items . (lambda () (not (org-at-table-p))))
    (cc/context-menu-buffers-items . cc/org-at-table-or-use-region-p)
    (cc/context-menu-narrow-items . (lambda () (org-at-table-p)))
    (cc/context-menu-workflow-items .  cc/org-at-table-or-use-region-p)
    (cc/context-menu-open-in-items . (lambda () (or
                                            (derived-mode-p 'dired-mode)
                                            (cc/org-at-table-or-use-region-p))))
    (cc/context-menu-dictionary-items . (lambda () (not (use-region-p))))
    (cc/context-menu-occur-items . (lambda () (not (use-region-p))))
    (cc/context-menu-vc-items . (lambda () (or
                                       (not
                                        (vc-responsible-backend
                                         default-directory t))
                                       (use-region-p))))
    (cc/context-menu-region-actions-items . (lambda () (not (use-region-p))))
    (cc/context-menu-markup-items  . (lambda () (use-region-p)))
    ;; (cc/context-menu-timekeeping-items (org-at-table-p))
    (cc/context-menu-word-count-items . (lambda () (or (org-at-table-p)
                                                  (not (derived-mode-p 'text-mode))))))
  "A list of function-predicate pairs.

Each element is a cons cell (FUNCTION . INAPT) where:
- FUNCTION is a callable that will be executed
- INAPT is a callable that returns non-nil when FUNCTION should not run"
  :type '(repeat (cons :tag "Function and Inapt"
                       (function :tag "Function")
                       (function :tag "Inapt")))
  :group 'kickingvegas)


;; -------------------------------------------------------------------
;; Predicates

(defun cc/org-at-table-or-use-region-p ()
  "Predicate if `org-at-table-p' or `use-region-p' are t."
  (or (org-at-table-p) (use-region-p)))



;; -------------------------------------------------------------------
;; Hook Function

(defun cc/context-menu-addon-items (menu click)
  "Charles Choi context menu hook function using MENU and CLICK event.

MENU - menu to be configured.
CLICK - event"

  (save-excursion
    (mouse-set-point click)
    (mapc (lambda (item)
            (let ((fn (car item))
                  (inapt (cdr item)))
              (funcall fn menu (funcall inapt))))
          cc/context-menu-functions-and-predicates)

    (easy-menu-add-item menu nil cc/wgrep-menu))
  menu)

(add-hook 'context-menu-functions #'cc/context-menu-addon-items)


;; -------------------------------------------------------------------
;; CC Menu Item Functions

(defun cc/context-menu-word-count-items (menu &optional inapt)
  "Menu items to populate MENU for word count section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu count-words-separator)
    (if (use-region-p)
          (easy-menu-add-item menu nil ["Count Words in Region"
                                        count-words
                                        :help "Count words in region"])

        (easy-menu-add-item menu nil ["Count Words in Buffer"
                                      count-words
                                      :help "Count words in buffer"]))))

(defun cc/context-menu-markup-items (menu &optional inapt)
  "Menu items to populate MENU for reveal markup section if INAPT nil."
  (unless inapt
    (cond
     ((derived-mode-p 'org-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Images"
                           casual-org-toggle-images
                           :help "Toggle images"])

      (easy-menu-add-item menu nil
                          ["Show Markup"
                           visible-mode
                           :style toggle
                           :selected visible-mode
                           :help "Toggle making all invisible text \
temporarily visible (Visible mode)"]))

     ((derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu markdown-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Hide Markup"
                           markdown-toggle-markup-hiding
                           :style toggle
                           :selected markdown-hide-markup
                           :help "Toggle the display or hiding of markup"]))
     (t nil))))

(defun cc/context-menu-vc-items (menu &optional inapt)
  "Menu items to populate MENU for version control section if INAPT nil."
  (unless inapt
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
      casual-ediff-revision-from-menu
      :visible (and (bound-and-true-p buffer-file-name)
                    (vc-registered (buffer-file-name)))
      :help "Ediff this file with revision"])))

(defun cc/context-menu-region-actions-items (menu &optional inapt)
  "Menu items to populate MENU for region actions section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu transform-text-separator)
    (easy-menu-add-item menu nil cc/transform-text-menu)
    (easy-menu-add-item menu nil cc/region-operations-menu)
    (cond
     ((derived-mode-p 'prog-mode)
      (easy-menu-add-item menu nil
                          ["Toggle Comment"
                           comment-dwim
                           :help "Toggle comment on selected region"]))

     ((or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
      (easy-menu-add-item menu nil cc/emphasize-menu)))))

(defun cc/context-menu-timekeeping-items (menu &optional inapt)
  "Menu items to populate MENU for timekeeping section if INAPT nil."
  (unless inapt
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
  (unless inapt
    (cc/context-menu-item-separator menu journal-separator)
    (easy-menu-add-item menu nil ["Journal"
                                  status-report
                                  :help "Go to current day journal"])

    (easy-menu-add-item menu nil ["Agenda - All TODOs"
                                  (lambda () (interactive)(org-agenda nil "n"))
                                  :help "Show Org agenda with all TODO tasks"])

    (easy-menu-add-item menu nil ["Add Note"
                                  (lambda () (interactive)(org-capture nil "j"))
                                  :help "Add journal note"])

    (easy-menu-add-item menu nil ["Scratch"
                                  scratch-buffer
                                  :help "Switch to the *scratch* buffer."])))

(defun cc/context-menu-dictionary-items (menu &optional inapt)
  "Menu items to populate MENU for <replace> section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu dictionary-operations-separator)
    (easy-menu-add-item menu nil ["Look Up"
                                  osx-dictionary-search-word-at-point
                                  :label (cc/context-menu-label "Look Up")
                                  :help "Look up selected region in macOS dictionary"])))

(defun cc/context-menu-occur-items (menu &optional inapt)
  "Menu items to populate MENU for occur section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu occur-separator)
    (easy-menu-add-item menu nil
                            ["Find word in buffer (occur)"
                             cc/occur-selected-region
                             :label (cc/context-menu-label "Occur")
                             :help "Show all lines in the current buffer \
containing a match for selected word"])))

(defun cc/context-menu-dired-items (menu &optional inapt)
  "Menu items to populate MENU for Dired section if INAPT nil."
  (unless inapt
    ;;
    (cc/context-menu-item-separator menu trash-separator)
    (easy-menu-add-item menu nil
                        ["Move to Trash…"
                         dired-do-delete
                         :visible (file-writable-p
                                         (dired-file-name-at-point))
                         :help "Delete all marked files."])

    (cc/context-menu-item-separator menu dired-separator)

    ;; (easy-menu-add-item menu nil
    ;;                     ["Insert Subdir"
    ;;                      dired-maybe-insert-subdir
    ;;                      :visible (file-directory-p
    ;;                                      (dired-file-name-at-point))
    ;;                      :help "Insert subdir (sub-directory)"])

    ;; (easy-menu-add-item menu nil
    ;;                     ["Kill Subdir"
    ;;                      dired-kill-subdir
    ;;                      :visible (and (dired-current-directory)
    ;;                                    (not (dired-file-name-at-point)))
    ;;                      :help "Kill subdir (sub-directory)"])

    ;; (easy-menu-add-item menu nil
    ;;                     ["Hide Subdir"
    ;;                      dired-hide-subdir ; this is so fucking broken
    ;;                      ;; cc/toggle-subdir
    ;;                      :visible (and (dired-current-directory)
    ;;                                    (not (dired-file-name-at-point)))
    ;;                      :help "Hide subdir (sub-directory)"])

    (easy-menu-add-item menu nil
                        ["Insert Subdir"
                         dired-maybe-insert-subdir
                         :visible (file-directory-p
                                         (dired-file-name-at-point))
                         :help "Insert subdir (sub-directory)"])

    (easy-menu-add-item menu nil
                        ["Kill Subdir"
                         dired-kill-subdir
                         :visible (and (dired-current-directory)
                                       (not (dired-file-name-at-point)))
                         :help "Kill subdir (sub-directory)"])

    (easy-menu-add-item menu nil
                        ["Hide Subdir"
                         dired-hide-subdir
                         :visible (and (dired-current-directory)
                                       (not (dired-file-name-at-point)))
                         :help "Hide subdir (sub-directory)"])

    (easy-menu-add-item menu nil
                        ["Rename…"
                         dired-do-rename
                         :help "Rename or move file"])

    (easy-menu-add-item menu nil casual-dired-sort-menu)
    (easy-menu-add-item menu nil
                        ["Duplicate"
                         cc/dired-duplicate-file
                         :label (format "Duplicate “%s.%s”"
                                        (file-name-base (dired-get-filename))
                                        (file-name-extension (dired-get-filename)))
                         :help "Duplicate selected item"])

    (easy-menu-add-item menu nil
                        ["Omit Mode"
                         dired-omit-mode
                         :style toggle
                         :selected dired-omit-mode
                         :help "Omit mode"])

    (easy-menu-add-item menu nil
                        ["Hide Details"
                         dired-hide-details-mode
                         :style toggle
                         :selected dired-hide-details-mode
                         :help "Hide directory details"])

    (easy-menu-add-item menu nil
                        ["Dired…"
                         dired
                         :help "Open Dired"])

    (cc/context-menu-item-separator menu dired-finder-separator)

    (easy-menu-add-item menu nil
                        ["Open in Finder"
                         reveal-in-folder-at-point
                         :help "Open file (buffer) in Finder"])))

(defun cc/toggle-subdir ()
  (interactive)
  (if (dired-subdir-hidden-p (dired-current-directory))
      (dired-hide-subdir 1)
    (call-interactively #'dired-hide-subdir)))

(defun cc/context-menu-workflow-items (menu &optional inapt)
  "Menu items to populate MENU for workflow section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu capture-flow-separator)
    (easy-menu-add-item menu nil
                        ["New Workflow…"
                         org-capture
                         :help "Create new task or workflow via org-capture"])))

(defun cc/context-menu-buffers-items (menu &optional inapt)
  "Menu items to populate MENU for buffers section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu buffer-navigation-separator)

    (easy-menu-add-item menu nil ["≣ List All Buffers"
                                  ibuffer
                                  :help "List all buffers"])

    (easy-menu-add-item menu nil ["← Buffer"
                                  previous-buffer
                                  :help "Go to previous buffer"])

    (easy-menu-add-item menu nil ["→ Buffer"
                                  next-buffer
                                  :help "Go to next buffer"])))


(defun cc/context-menu-open-in-items (menu &optional inapt)
  "Menu items to populate MENU for open in section if INAPT nil."
  (unless inapt
    (cc/context-menu-item-separator menu open-in-separator)

    (easy-menu-add-item menu nil
                        ["Open in Finder"
                         reveal-in-folder-this-buffer
                         :visible (buffer-file-name)
                         :help "Open file (buffer) in Finder"])

    (easy-menu-add-item menu nil
                        ["Open in Dired"
                         dired-jump-other-window
                         :visible (buffer-file-name)
                         :help "Open file in Dired"])))

(defun cc/context-menu-narrow-items (menu &optional inapt)
  "Menu items to populate MENU for narrow section if INAPT nil."
  (unless inapt
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


(easy-menu-define cc/org-table-region-menu nil
  "Key map for Org table region sub-menu."
  '("Org Table Region"
    ["Cut"
     org-table-cut-region
     :enable (and (bound-and-true-p rectangle-mark-mode) (use-region-p))
     :help "Cut Org table region"]

    ["Copy"
     org-table-copy-region
     :enable (and (bound-and-true-p rectangle-mark-mode) (use-region-p))
     :help "Copy Org table region"]

    ["Paste"
     org-table-paste-rectangle
     :help "Paste Org table region"]))

(defun cc/context-menu-org-table-items (menu &optional inapt)
  "Menu items to populate MENU for Org table section if INAPT nil.

Use C-M-Drag-mouse-1 to make a rectangular selection. In the event only
M-Drag-mouse-1 (set secondary selection) is sent, use M-Drag-mouse-1 to
clear it."
  (unless inapt
    (cc/context-menu-item-separator menu org-table-sqeparator)
    (easy-menu-add-item menu nil
                        ["Table Cell Info"
                         casual-org-table-copy-reference-dwim
                         :label (casual-org-table--reference-dwim)
                         :help "Copy Org table reference (field or range) into kill ring via mouse"])

    (easy-menu-add-item menu nil cc/org-table-region-menu)

    (easy-menu-add-item menu nil
                        ["Show Coordinates"
                         org-table-toggle-coordinate-overlays
                         :style toggle
                         :selected org-table-coordinate-overlays
                         :help "Toggle the display of row/column numbers in tables"])

    (easy-menu-add-item menu nil
                        ["Edit Table Formulas"
                         org-table-edit-formulas
                         :help "Edit the formulas of the current table in a separate buffer."])
    (easy-menu-add-item menu nil cc/insert-org-plot-menu)
    (easy-menu-add-item menu nil ["Run gnuplot"
                                  org-plot/gnuplot
                                  :help "Plot table using gnuplot"])))



(defun cc/context-menu-region (menu click)
  "Region menu using MENU and CLICK."

  (save-excursion
    (mouse-set-point click)
    (cond
     ((derived-mode-p 'org-mode)
      (easy-menu-add-item menu nil cc/org-copy-as-menu)
      (easy-menu-add-item menu nil
                          ["Paste Last Org Link"
                           org-insert-last-stored-link
                           :enable (cc/org-stored-links-p)
                           :help "Insert the last link stored in org-stored-links"])

      (easy-menu-add-item menu nil
                          ["Paste Markdown"
                           cc/yank-markdown-as-org
                           :help "Paste Markdown"])

      ;; TODO: need test to see if media is there to paste
      (easy-menu-add-item menu nil
                          ["Paste Media"
                           yank-media
                           :visible (and (derived-mode-p 'org-mode) (cc/yank-media-p))
                           :help "Paste (yank) media"]))))
  menu)


(easy-menu-define cc/org-copy-as-menu nil
  "Key map for Org copy sub-menu."
  '("Copy as…"
    :visible (and (derived-mode-p 'org-mode) (use-region-p))

    ["Markdown"
     mb/org-copy-region-as-markdown
     :help "Copy region as Markdown"]

    ["Slack"
     org-slack-export-to-clipboard-as-slack
     :help "Copy as Slack to clipboard"]

    ["RTF"
     dm/copy-as-rtf
     :help "Copy as RTF to clipboard"]))


(defun cc/insert-into-context-menu-functions (source target)
  "Insert SOURCE before TARGET in `context-menu-functions'.

This function provides finer grained control in inserting a context menu
function into `context-menu-functions' over `add-hook'."
  (let* ((s (default-value 'context-menu-functions))
         (i (seq-position s target)))

    (setq s (append (seq-subseq s 0 i)
                    (cons source (seq-subseq s i))))
    (setq-default context-menu-functions s)))

(defun cc/remove-from-context-menu-functions (target)
  "Remove TARGET in `context-menu-functions'."
  (let* ((s (default-value 'context-menu-functions)))

    (setq s (remove target s))
    (setq-default context-menu-functions s)))

(cc/insert-into-context-menu-functions #'cc/context-menu-region
                                       #'context-menu-middle-separator)

(cc/remove-from-context-menu-functions #'context-menu-minor)
(cc/remove-from-context-menu-functions #'context-menu-local)
(cc/remove-from-context-menu-functions #'context-menu-middle-separator)

(provide 'cc-context-menu)
;;; cc-context-menu.el ends here
