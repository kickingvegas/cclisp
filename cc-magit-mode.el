;;; cc-magit-mode.el --- Magit configuration         -*- lexical-binding: t; -*-

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

(require 'magit)
(require 'git-link-transient)
(require 'casual-lib)
(require 'cc-gh)

(add-hook 'magit-status-mode-hook (lambda () (toggle-truncate-lines -1)))

(keymap-set magit-status-mode-map "<f1>" #'git-link-homepage)
(keymap-set magit-status-mode-map "C-c m" #'git-link-dispatch)


(transient-define-prefix cc/magit-tmenu ()
  [["Magit"
    ("b" "Branch›" magit-branch)
    ("l" "Log›" magit-log)
    ("f" "Pull›" magit-pull)
    ("p" "Push›" magit-push)]

   ["gh"
    ("i" "List Issues" cc/gh-list-issues-vtable)]

   ["Misc"
    ("c" "Compile…" compile)
    ("d" "Dired" dired-jump-other-window)
    ("J" "Jump…" bookmark-jump)]
   ]

  [:class transient-row
    (casual-lib-quit-one)
    (casual-lib-quit-all)
    ("q" "Quit" magit-mode-bury-buffer)])

(keymap-set magit-status-mode-map "C-o" #'cc/magit-tmenu)

(provide 'cc-magit-mode)
;;; cc-magit-mode.el ends here
