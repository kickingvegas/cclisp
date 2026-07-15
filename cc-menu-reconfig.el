;;; cc-menu-reconfig.el --- Menu reconfiguration -*- lexical-binding: t; -*-

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

;; Menu Settings
(require 'text-mode)
(require 'vc)
(require 'helm)
(require 'dired)
(require 'transpose-frame)
(require 'cc-region-operations-menu)
(require 'cc-truth-table)
(require 'eww)
(require 'casual-bookmarks)
(require 'anju-main-menu)
(require 'anju-mode-line)


(defun cc/dired-side-right (path)
  "Side-by-side layout with Dired buffer on the right set to PATH."
  (interactive "DDirectory: ")
  (delete-other-windows)
  (dired-other-window path)
  (transpose-frame)
  (other-window 1))

(defun cc/set-input-method-hangul nil
  "Set input method to 한글."
  (interactive)
  (set-input-method 'korean-hangul))



;; -------------------------------------------------------------------
;;; Reconfigure Tools Menu
(defun cc/reconfigure-tools-menu ()
  "Reconfigure Tools menu."

  (easy-menu-add-item global-map '(menu-bar tools)
                      [count-words
                       count-words
                       :label "Count Words"
                       :visible (derived-mode-p 'text-mode)
                       :help "Count words in buffer or region if active"]
                      'grep)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [cc/set-input-method-hangul
                       cc/set-input-method-hangul
                       :label "Set Input Method - 한글"
                       :enable (not current-input-method)
                       :help "Set input method to 한글"]
                      'shell-commands)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [gah-issues
                       gah-issues
                       :label "GitHub Issues…"
                       :help "Put current issues for a GitHub repository in a vtable"]
                      'grep)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [cc/org-search
                       cc/org-search
                       :label "Search Org Notes…"
                       :help "Search Org Notes in ~/org"]
                      'shell-commands)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [cc/org-babel-ingest-table-to-sql
                       cc/org-babel-ingest-table-to-sql
                       :label "Babel Ingest - Org Table To SQL"
                       :visible (derived-mode-p 'org-mode)
                       :help "Ingest code block to convert Org Table to SQLite"]
                      'games)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [cc/insert-truth-table-input
                       cc/insert-truth-table-input
                       :label "Insert Truth Table…"
                       :visible (not buffer-read-only)
                       :help "Insert truth table input with 2^BITS rows into current buffer"]
                      'shell-commands)

  (keymap-set-after (lookup-key global-map [menu-bar tools])
    "<separator-shell>"
    '(menu-item "--")
    'cc/insert-truth-table-input)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [magit-status
                       magit-status
                       :label "Magit Status"
                       :visible (vc-responsible-backend default-directory t)
                       :help "Show the status of the current Git repository \
in a buffer"]
                      'vc)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [eshell
                       eshell
                       :label "Eshell"
                       :help "Create an interactive Eshell buffer"]
                      'calendar)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [ielm
                       ielm
                       :label "IELM"
                       :help "Interactively evaluate Emacs Lisp expressions"]
                      'calendar)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [run-python
                       run-python
                       :label "Python REPL"
                       :help "Run an inferior Python process"]
                      'calendar)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [swift-repl
                       swift-repl
                       :label "Swift REPL"
                       :help "Run the Swift REPL"]
                      'calendar)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [node-repl
                       node-repl
                       :label "NodeJS REPL"
                       :help "Run the NodeJS REPL"]
                      'calendar)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [re-builder
                       re-builder
                       :label "RE-Builder"
                       :help "Construct a regexp interactively"]
                      'calendar)

  (keymap-set-after (lookup-key global-map [menu-bar tools])
    "<separator-re>"
    '(menu-item "--")
    're-builder)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [world-clock
                       world-clock
                       :label "World Clock"
                       :help "Display a world clock buffer with times in \
various time zones"]
                      'calc)

  (easy-menu-add-item global-map '(menu-bar tools)
                      [eww
                       eww
                       :label "EWW…"
                       :help "Open EWW browser"]
                      'games)



  (keymap-set-after (lookup-key global-map [menu-bar tools])
    "<separator-games>"
    '(menu-item "--")
    'eww)

  (cc/tools-menu-reset))


(defun cc/tools-menu-reset ()
  "Reset main menu Tools menu."
  (let ((remove-list '(grep
                       rgrep
                       ede
                       semantic
                       compile
                       gdb
                       gnus
                       rmail
                       compose-mail
                       directory-search
                       browse-web
                       separator-net
                       encryption-decryption
                       separator-encryption-decryption
                       Table
                       separator-spell
                       spell)))

    (mapc (lambda (x)
            (let ((path (vector 'menu-bar 'tools x)))
              (if (lookup-key global-map path)
                  (define-key global-map path nil t))))
          remove-list)))

(defun cc/org-babel-ingest-table-to-sql ()
  "Support SQL table operations on an Org table."
  (interactive)
  (org-babel-lob-ingest "~/org/babel/cc-org-table-to-sql.org"))

(provide 'cc-menu-reconfig)

;;; cc-menu-reconfig.el ends here
