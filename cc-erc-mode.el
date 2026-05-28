;;; cc-erc-mode.el --- ERC customization             -*- lexical-binding: t; -*-

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
(require 'bookmark)
(require 'erc-nicks)
(require 'erc-backend)
(require 'goto-addr)
(require 'casual-editkit)

(add-hook 'erc-mode-hook #'goto-address-mode)

(transient-define-prefix casual-erc-tmenu ()
  "Transient menu for ERC."

  ["Casual: ERC"
   ["Channel"
    ("s" "Switch…" erc-switch-to-buffer :transient t)
    ("x" "Clear" erc-kill-input)
    ("a" "BoL" erc-bol)]

   ["Edit"
    ("e" "Edit›" casual-editkit-edit-tmenu)
    ("E" "Emoji & Symbols›" casual-editkit-emoji-symbols-tmenu
     :if-not casual-editkit-buffer-read-only-p)
    ("B" "Bookmarks›" casual-editkit-bookmarks-tmenu)
    ]

   ["Sexp"
    ("m" "Mark" mark-sexp)
    ("c" "Copy" casual-editkit-copy-sexp)
    ("k" "Kill (Cut)" kill-sexp
     :if-not casual-editkit-buffer-read-only-p)
    ("t" "Transpose" transpose-sexps
     :if-not casual-editkit-buffer-read-only-p)]

   ["Navigation"
    ("<prior>" "Page Up" scroll-down-command :transient t)
    ("<next>" "Page Down" scroll-up-command :transient t)]

   ["Misc"
    ("n" "Names" erc-channel-names :transient t)
    ("f" "Toggle Flood" erc-toggle-flood-control :transient t)
    ("b" "Toggle Bufbar" erc-bufbar-mode :transient t)]]

  [("J" "Jump to Bookmark…" bookmark-jump)]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Dismiss" transient-quit-all)
          (casual-lib-quit-all)
          ("Q" "Quit" erc-quit-server)])

(defun cc/configure-erc-tty ()
  "Set ERC nickname background to black on TTY."
  (if (not (display-graphic-p))
      (setopt erc-nicks-bg-color "black")))

(keymap-set erc-mode-map "M-m" #'casual-erc-tmenu)
(keymap-set erc-mode-map "C-c m" #'casual-erc-tmenu)
(keymap-set erc-mode-map "C-o" #'casual-erc-tmenu)
(keymap-set erc-mode-map "<f1>" #'erc-switch-to-buffer)

(cc/configure-erc-tty)

(defun cc/redact (str)
  "Redact STR."
  (let* ((first (substring str 0 1))
         (last (substring str -1))
         (count (length str))
         (result (concat first (make-string (- count 2) ?*) last)))
    result))

(defun cc/erc-redact (_ parsed)
  (let* ((msg (erc-response.contents parsed))
         (expletives '("Nigger" "Kike" "Chink" "Fag" "Faggot" "Gook" "Coon")))
    (when (stringp msg)
      (dolist (e expletives)
        (setq msg (replace-regexp-in-string e (cc/redact e) msg)))
      (setf (erc-response.contents parsed) msg)
      nil)))

(add-hook 'erc-server-PRIVMSG-functions #'cc/erc-redact)

(provide 'cc-erc-mode)
;;; cc-erc-mode.el ends here
