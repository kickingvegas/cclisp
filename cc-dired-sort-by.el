;;; cc-dired-sort-by.el --- Dired Sort By            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: unix, tools

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

;; cc-dired-sort-by is a set of functions that provide richer sorting
;; capability to Dired mode. Two means are provided:
;;
;; - Transient menu for a keyboard-driven workflow
;; - Context menu for a mouse-driven workflow
;;
;; * Installation
;;
;; To use the Transient menu, ensure that you have these statements in your
;; init file.
;;
;; (require 'cc-dired-sort-by)
;; (define-key dired-mode-map (kbd "s") 'cc/dired-sort-by)
;;
;; The keymap `cc/dired-sort-menu' can be used in a context menu
;; (see `context-menu-mode') or some other main menu.
;;
;; Typically this is done with code looking like this:
;;
;; (defun cc/context-menu-addons (menu click)
;;   (easy-menu-add-item menu nil cc/dired-sort-menu))
;;
;; (add-hook 'context-menu-functions #'cc/context-menu-addons)

;;; Code:
(require 'dired)
(require 'transient)
(require 'easymenu)

(defcustom cc-dired-listing-switches '("--human-readable"
                                       "--group-directories-first"
                                       "--time-style=long-iso")
  "List of GNU ls arguments used by the function `cc/--dired-sort-by'.

This variable requires GNU ls from coreutils installed.\n
See the man page `ls(1)' for details."
  :type '(repeat string)
  :group 'dired)

(transient-define-prefix cc/dired-sort-by ()
  "Transient menu to sort Dired buffer by different criteria.

This function requires GNU ls from coreutils installed."
  :value '("--human-readable"
           "--group-directories-first"
           "--time-style=long-iso")
                                     ; TODO: support cc-dired-listing-switches
  [["Arguments"
    ("-a" "all" "--all")
    ("g" "group directories first" "--group-directories-first")
    ("-r" "reverse" "--reverse")
    ("-h" "human readable" "--human-readable")
    ("t" "time style" "--time-style="
     :choices ("full-iso" "long-iso" "iso" "locale"))]

   ["Sort By"
    ("n"
     "Name"
     (lambda () (interactive)
       (cc/--dired-sort-by :name
                           (transient-args transient-current-command)))
     :transient nil)
    ("k"
     "Kind"
     (lambda () (interactive)
       (cc/--dired-sort-by :kind
                           (transient-args transient-current-command)))
     :transient nil)
    ("l"
     "Date Last Opened"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-last-opened
                           (transient-args transient-current-command)))
     :transient nil)
    ("a"
     "Date Added"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-added
                           (transient-args transient-current-command)))
     :transient nil)
    ("m"
     "Date Modified"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-modified
                           (transient-args transient-current-command)))
     :transient nil)
    ("M"
     "Date Metadata Changed"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-metadata-changed
                           (transient-args transient-current-command)))
     :transient nil)
    ("v"
     "Version"
     (lambda () (interactive)
       (cc/--dired-sort-by :version
                           (transient-args transient-current-command)))
     :transient nil)
    ("s"
     "Size"
     (lambda () (interactive)
       (cc/--dired-sort-by :size
                           (transient-args transient-current-command)))
     :transient nil)]])

(defun cc/--dired-sort-by (criteria &optional prefix-args)
  "Sort current Dired buffer according to CRITERIA and PREFIX-ARGS.

This function will invoke `dired-sort-other' with arguments built from
CRITERIA and PREFIX-ARGS.

CRITERIA is a keyword of which the following are supported:
  :name             :date-added             :version
  :kind             :date-metadata-changed  :size
  :date-last-opened :date-modified

PREFIX-ARGS is a list of GNU ls arguments. If nil, then it will use the value
of `cc-dired-listing-switches'. Otherwise this is typically populated by the
Transient menu `cc/dired-sort-by'.

This function requires GNU ls from coreutils installed.

See the man page `ls(1)' for details."
  (when dired-sort-inhibit
    (error "Cannot sort this Dired buffer"))
  (let ((arg-list (list "-l")))
    (if prefix-args
        (nconc arg-list prefix-args)
      (nconc arg-list cc-dired-listing-switches))
    (cond
     ((eq criteria :name)
      (message "Sorted by name"))

     ((eq criteria :kind)
      (message "Sorted by kind")
      (push "--sort=extension" arg-list))

     ((eq criteria :date-last-opened)
      (message "Sorted by date last opened")
      (push "--sort=time" arg-list)
      (push "--time=access" arg-list))

     ((eq criteria :date-added)
      (message "Sorted by date added")
      (push "--sort=time" arg-list)
      (push "--time=creation" arg-list))

     ((eq criteria :date-modified)
      (message "Sorted by date modified")
      (push "--sort=time" arg-list)
      (push "--time=modification" arg-list))

     ((eq criteria :date-metadata-changed)
      (message "Sorted by date metadata changed")
      (push "--sort=time" arg-list)
      (push "--time=status" arg-list))

     ((eq criteria :version)
      (message "Sorted by version")
      (push "--sort=version" arg-list))

     ((eq criteria :size)
      (message "Sorted by size")
      (push "-S" arg-list))

     (t
      (message "Default sorted by name")))

    (dired-sort-other (mapconcat 'identity arg-list " "))))

(easy-menu-define cc/dired-sort-menu nil
  "Keymap for Dired sort by menu."
  '("Sort By"
    :visible (and (derived-mode-p 'dired-mode) (not dired-sort-inhibit))
    ["Name"
     (lambda () (interactive) (cc/--dired-sort-by :name))
     :help "Sort by name"]
    ["Kind"
     (lambda () (interactive) (cc/--dired-sort-by :kind))
     :help "Sort by kind"]
    ["Date Last Opened"
     (lambda () (interactive) (cc/--dired-sort-by :date-last-opened))
     :help "Sort by date last opened"]
    ["Date Added"
     (lambda () (interactive) (cc/--dired-sort-by :date-added))
     :help "Sort by date added"]
    ["Date Modified"
     (lambda () (interactive) (cc/--dired-sort-by :date-modified))
     :help "Sort by date modified"]
    ["Date Metadata Changed"
     (lambda () (interactive) (cc/--dired-sort-by :date-metadata-changed))
     :help "Sort by date metadata changed"]
    ["Version"
     (lambda () (interactive) (cc/--dired-sort-by :version))
     :help "Sort by version"]
    ["Size"
     (lambda () (interactive) (cc/--dired-sort-by :size))
     :help "Sort by size"]
    "--"
    ("Reverse Sort By"
     ["Name"
      (lambda () (interactive) (cc/--dired-sort-by
                                :name
                                (append '("--reverse") cc-dired-listing-switches)))

      :help "Reverse sort by name"]
     ["Kind"
      (lambda () (interactive) (cc/--dired-sort-by
                                :kind
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by kind"]
     ["Date Last Opened"
      (lambda () (interactive) (cc/--dired-sort-by
                                :date-last-opened
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date last opened"]
     ["Date Added"
      (lambda () (interactive) (cc/--dired-sort-by
                                :date-added
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date added"]
     ["Date Modified"
      (lambda () (interactive) (cc/--dired-sort-by
                                :date-modified
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date modified"]
     ["Date Metadata Changed"
      (lambda () (interactive) (cc/--dired-sort-by
                                :date-metadata-changed
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by date metadata changed"]
     ["Version"
      (lambda () (interactive) (cc/--dired-sort-by
                                :version
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by version"]
     ["Size"
      (lambda () (interactive) (cc/--dired-sort-by
                                :size
                                (append '("--reverse") cc-dired-listing-switches)))
      :help "Reverse sort by size"])))

(provide 'cc-dired-sort-by)
;;; cc-dired-sort-by.el ends here
