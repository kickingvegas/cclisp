;;; cc-info-mode.el --- Info mode customization      -*- lexical-binding: t; -*-

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
(require 'info)
(require 'transient)
(require 'bookmark)
(require 'ibuffer)
(require 'cclisp)
(require 'cc-main-tmenu)

(transient-define-prefix cc/info-tmenu ()
  "CC Info Transient menu."

  [["Overview"
    ("d" "Directory" Info-directory :transient nil)
    ("t" "Top" Info-top-node)
    ("T" "ToC" Info-toc :transient nil)]

   ["Goto"
    ("m" "Menu…" Info-menu)
    ("g" "Node…" Info-goto-node)
    ("i" "Index…" Info-index)
    ("I" "V-index…" Info-virtual-index)]

   ["Search"
    :pad-keys t
    ("C-s" "I-search…" isearch-forward)
    ("s" "Search…" Info-search)
    ("S" "Case sensitive…" Info-search-case-sensitively)]

   ["History"
    :pad-keys t
    ("L" "History" Info-history :transient nil)
    ("M-[" "⏪️" Info-history-back
     :description (lambda () (cc/info-unicode-db-get :rewind))
     :transient t)
    ("M-]" "⏩️" Info-history-forward
     :description (lambda () (cc/info-unicode-db-get :fast-forward))
     :transient t)]]

  ["Navigation"
   ["Link"
    :description (lambda () (cc/info-unicode-db-get :link))
    ("k" "Previous" Info-prev-reference
     :description (lambda () (cc/info-unicode-db-get :up-arrow))
     :transient t)
    ("j" "Next" Info-next-reference
     :description (lambda () (cc/info-unicode-db-get :down-arrow))
     :transient t)]

   ["Paragraph"
    :description (lambda () (cc/info-unicode-db-get :paragraph))
    ("p" "Previous" cc/browse-backward-paragraph
     :description (lambda () (cc/info-unicode-db-get :up-arrow))
     :transient t)
    ("n" "Next" cc/browse-forward-paragraph
     :description (lambda () (cc/info-unicode-db-get :down-arrow))
     :transient t)]

   ["All Nodes"
    ("[" "⏪️⤴️" Info-backward-node
     :description (lambda ()
                    (cc/info-unicode-db-get :rewind-or-up))
     :transient t)
    ("]" "⏩️⤵️" Info-forward-node
     :description (lambda ()
                    (cc/info-unicode-db-get :fast-forward-or-down))
     :transient t)]

   ["Peer Nodes"
    ("h" "⏪️" Info-prev
     :description (lambda () (cc/info-unicode-db-get :rewind))
     :transient t)
    ("l" "⏩️" Info-next
     :description (lambda () (cc/info-unicode-db-get :fast-forward))
     :transient t)]

   [""
    ("<" "⏮️" Info-top-node
     :description (lambda () (cc/info-unicode-db-get :first))
     :transient t)
    (">" "⏭️" Info-final-node
     :description (lambda () (cc/info-unicode-db-get :last))
     :transient t)]

   [""
    ("^" "⏫️"  Info-up
     :description (lambda () (cc/info-unicode-db-get :up))
     :transient t)]]

  ["Quick"
   [("J" "Jump to bookmark…" bookmark-jump :transient nil)
    ("B" "Set bookmark…" bookmark-set-no-overwrite :transient nil)
    ("b" "List buffers" ibuffer :transient nil)]

   [("c" "Copy node name" Info-copy-current-node-name :transient nil)
    ("G" "Open node in web…" Info-goto-node-web :transient nil)]

   [:pad-keys t
              ("C-M-n" "New frame" (lambda ()
                                         (interactive)
                                         (clone-frame nil t))
               :transient nil)
              ("M-n" "Clone buffer" clone-buffer :transient nil)]]

  [:class transient-row
          ("<return>" "Open" Info-follow-nearest-node :transient t)
          ("<space>" "⏩️⤵️" Info-scroll-up
           :description (lambda ()
                    (cc/info-unicode-db-get :fast-forward-or-down))
           :transient t)
          ("q" "Dismiss" ignore :transient transient--do-exit)])


(defconst cc/info-unicode-db
  '((:fast-forward . '("⏩️" "Next"))
    (:rewind . '("⏪️" "Previous"))
    (:fast-forward-or-down . '("⏩️⤵️"  "Next or down"))
    (:rewind-or-up . '("⏪️⤴️" "Previous or up"))
    (:up . '("⏫️" "Up"))
    (:first . '("⏮️" "First"))
    (:last . '("⏭️" "Last"))
    (:up-arrow . '("↑" "Previous"))
    (:down-arrow . '("↓" "Next"))
    (:paragraph . '(" ¶" "Paragraph"))
    (:link . '(" 🔗" "Link")))
  "Unicode symbol DB to use for Info Transient menus.")

(defun cc/info-unicode-db-get (key &optional db)
  "Lookup Unicode symbol for KEY in DB.

- KEY symbol used to lookup Unicode symbol in DB.
- DB alist containing Unicode symbols used by Info Transient menus.

If DB is nil, then `cc/info-unicode-db' is used by default.

If the value of customizable variable `cchoi-use-unicode-symbols'
is non-nil, then the Unicode symbol is returned, otherwise a
plain ASCII-range string."
  (let* ((db (or db cc/info-unicode-db))
         (unicode cchoi-use-unicode-symbols)
         (item (alist-get key db)))
    (if unicode
        (nth 0 (eval item))
      (nth 1 (eval item)))))


(keymap-set Info-mode-map "C-o" #'cc/info-tmenu)
(keymap-set Info-mode-map "C-M-o" #'cc/main-tmenu)

(provide 'cc-info-mode)
;;; cc-info-mode.el ends here
