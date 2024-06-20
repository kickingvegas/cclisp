;;; cc-bookmarks-bmenu-mode.el --- Bookmarks Bmenu Mode Config  -*- lexical-binding: t; -*-

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
(require 'hl-line)
(require 'bookmark)
(require 'transient)
(require 'tabulated-list)
(require 'cc-main-tmenu)
(require 'casual-lib)

(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(transient-define-prefix cc/bookmark-bmenu-tmenu ()
  "CC Bookmark Transient menu."

  [["Open"
    ("o" "Other window" bookmark-bmenu-other-window :transient nil)
    ("v" "Marked"  bookmark-bmenu-select :transient nil)]

   ["Edit"
    ("r" "Rename…" bookmark-bmenu-rename :transient nil)
    ("R" "Relocate…" bookmark-bmenu-relocate :transient nil)]

   ["Mark"
    ("m" "Mark" bookmark-bmenu-mark :transient t)
    ("M" "Mark all" bookmark-bmenu-mark-all :transient t)
    ("u" "Unmark" bookmark-bmenu-unmark :transient t)
    ("U" "Unmark all" bookmark-bmenu-unmark-all :transient t)]

   ["Misc"
    ("/" "Filter…" bookmark-bmenu-search :transient t)
    ("w" "Show location" bookmark-bmenu-locate :transient t)]

   ["Navigate"
    ("p" "↑" previous-line :transient t)
    ("n" "↓" next-line :transient t)
    ("<" "⤒" beginning-of-buffer :transient t)
    (">" "⤓" end-of-buffer :transient t)]]

  [["Annotation"
    ("e" "Edit" bookmark-bmenu-edit-annotation :transient nil)
    ("a" "Show" bookmark-bmenu-show-annotation :transient nil)
    ("A" "Show All" bookmark-bmenu-show-all-annotations :transient nil)]

   ["Delete"
    ("d" "To delete" bookmark-bmenu-delete :transient t)
    ("D" "All to delete" bookmark-bmenu-delete-all :transient t)
    ("x" "Delete marked" bookmark-bmenu-execute-deletions :transient t)]

   ["Display"
    ("s" "Sort by›" cc/bookmark-bmenu-sortby-tmenu :transient t)
    ("t" "Toggle locations" bookmark-bmenu-toggle-filenames :transient t)]

   ["Column"
    ("b" "←" tabulated-list-previous-column :transient t)
    ("f" "→" tabulated-list-next-column :transient t)
    ("{" "Narrow Column" tabulated-list-narrow-current-column :transient t)
    ("}" "Widen Column" tabulated-list-widen-current-column :transient t)]]

  [:class transient-row
          ("<return>" "Open" bookmark-bmenu-this-window :transient nil)
          ("g" "Refresh" revert-buffer :transient t)
          ("S" "Save" bookmark-bmenu-save :transient t)
          (casual-lib-quit-all)])

(transient-define-prefix cc/bookmark-bmenu-sortby-tmenu ()
  ["Sort By"
   ("n" "Name" cc/bookmark-bmenu-sortby-name :transient nil)
   ("m" "Last Modified" cc/bookmark-bmenu-sortby-modified :transient nil)
   ("c" "Creation Time" cc/bookmark-bmenu-sortby-creation :transient nil)]
  [:class transient-row
          (casual-lib-quit-all)])

(defun cc/bookmark-bmenu-sortby-name ()
  "Sort bookmark list by name."
  (interactive)
  (setq-local bookmark-sort-flag t)
  (revert-buffer))

(defun cc/bookmark-bmenu-sortby-modified ()
  "Sort bookmark list by last modified time."
  (interactive)
  (setq-local bookmark-sort-flag 'last-modified)
  (revert-buffer))

(defun cc/bookmark-bmenu-sortby-creation ()
  "Sort bookmark list by creation time."
  (interactive)
  (setq-local bookmark-sort-flag nil)
  (revert-buffer))

;; Streamlined Bookmarks Menu
(easy-menu-define cc/bookmarks-menu nil
  "Keymap for CC Bookmarks Menu."
  '("Bookmarks"
    ["Edit Bookmarks" list-bookmarks
     :help "Display a list of existing bookmarks."]
    "---"
    ["Add Bookmark…" bookmark-set-no-overwrite
     :help "Set a bookmark named NAME at the current location."]
    "---"
    ["Jump to Bookmark…" bookmark-jump
     :help "Jump to bookmark"]))

(easy-menu-add-item global-map '(menu-bar)
                    cc/bookmarks-menu
                    "Tools")

(keymap-set bookmark-bmenu-mode-map "C-o" #'cc/bookmark-bmenu-tmenu)
(keymap-set bookmark-bmenu-mode-map "C-M-o" #'cc/main-tmenu)

(provide 'cc-bookmarks-bmenu-mode)
;;; cc-bookmarks-bmenu-mode.el ends here
