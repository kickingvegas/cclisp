;;; cc-eww-mode.el --- EWW Mode                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'eww)
(require 'hl-line)
(require 'bookmark)
(require 'casual-lib)

(add-hook 'eww-mode-hook #'hl-line-mode)

(transient-define-prefix casual-eww-tmenu ()
   "Transient menu for eww."
   :refresh-suffixes t
   ["Casual EWW"
    ["History"
     :pad-keys t
     ("M-[" "Previous" eww-forward-url :transient t)
     ("M-]" "Next" eww-back-url :transient t)
     ("H" "History" eww-list-histories :transient nil)]

    ["Document"
     ("[" "Back" eww-previous-url :transient t)
     ("]" "Next" eww-next-url :transient t)
     ("^" "Up" eww-up-url :transient t)
     ("t" "Top" eww-top-url :transient t)]

    ["Navigate"
     :pad-keys t
     ("p" "↑ ¶" casual-lib-browse-backward-paragraph :transient t)
     ("n" "↓ ¶" casual-lib-browse-forward-paragraph :transient t)
     ("SPC" "↓ Scroll" scroll-up-command :transient t)
     ("S-SPC" "↑ Scroll" scroll-down-command :transient t)]

    ["Link"
     :pad-keys t
     ("j" "Next" shr-next-link :transient t)
     ("k" "Previous" shr-previous-link :transient t)
     ("RET" "Follow" eww-follow-link :transient t)]

    ["Bookmarks"
     :pad-keys t
     ("ba" "Add" eww-add-bookmark)
     ("B" "List" eww-list-bookmarks)
     ("bn" "Next" eww-next-bookmark)
     ("bp" "Previous" eww-previous-bookmark)
     ]

    ["Misc"
     :pad-keys t
     ("c" "Copy URL" eww-copy-page-url)
     ("a" "Copy Alt URL" eww-copy-alternate-url)
     ("M-l" "Open URL" eww)
     ("C-o" "Launch External" eww-browse-with-external-browser)
     ("g" "Reload" eww-reload)
     ("J" "Jump to Bookmark…" bookmark-jump)]]

   [:class transient-row
           (casual-lib-quit-one)
           ("q" "Quit" quit-window)
           (casual-lib-quit-all)])

(keymap-set eww-mode-map "C-o" #'casual-eww-tmenu)
(keymap-set eww-mode-map "C-c C-o" #'eww-browse-with-external-browser)

(keymap-set eww-mode-map "j" #'shr-next-link)
(keymap-set eww-mode-map "k" #'shr-previous-link)

(keymap-set eww-mode-map "[" #'eww-previous-url)
(keymap-set eww-mode-map "]" #'eww-next-url)

(keymap-set eww-mode-map "M-]" #'eww-forward-url)
(keymap-set eww-mode-map "M-[" #'eww-back-url)

(keymap-set eww-mode-map "n" #'casual-lib-browse-forward-paragraph)
(keymap-set eww-mode-map "p" #'casual-lib-browse-backward-paragraph)
;;(keymap-set eww-mode-map "p" #'backward-paragraph)

;; (keymap-set eww-mode-map "n" #'next-line)
;; (keymap-set eww-mode-map "p" #'previous-line)

(keymap-set eww-mode-map "M-l" #'eww)

(provide 'cc-eww-mode)
;;; cc-eww-mode.el ends here
