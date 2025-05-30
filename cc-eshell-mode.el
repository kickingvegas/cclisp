;;; cc-eshell-mode.el --- eshell customization -*- lexical-binding: t; -*-

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
(require 'eshell)
(require 'esh-mode)
(require 'em-hist)
(require 'company)
(require 'hl-line)
(require 'helm-eshell)
(require 'eshell-git-prompt)
(require 'cclisp)
(require 'casual-lib)

(defvar eshell-mode-map)
(defvar eshell-visual-options)
(defvar eshell-visual-commands)
(defvar eshell-visual-subcommands)
(declare-function eshell/pwd "pwd" ())

(defun cc/prompt-function ()
        (concat "\n┏━ "
                (user-login-name) "@" (system-name) ":"
                (propertize (if (string= (eshell/pwd) (getenv "HOME"))
                                "~"
                              (replace-regexp-in-string
                               (concat "^" (getenv "HOME")) "~" (eshell/pwd)))
                            'face `(:foreground "orange red"))
                (if (and (not (file-remote-p default-directory))
                         (eshell-git-prompt--branch-name))
                    (format " (%s)" (eshell-git-prompt--branch-name))
                  "")
                "\n┗━━ "
                (if (= (user-uid) 0) "# " "$ ")))

(setopt eshell-prompt-function #'cc/prompt-function)
;;(setopt eshell-banner-message (format "Eshell ⌨️\n%s" (sunrise-sunset)))

(add-hook 'eshell-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook 'hl-line-mode)
(add-hook 'eshell-mode-hook (lambda ()
                              (keymap-set eshell-mode-map "<f1>" #'eshell-list-history)
			      ;;(keymap-set eshell-mode-map "<tab>" 'company-complete)
			      (keymap-set eshell-mode-map "C-r" 'helm-eshell-history)
                              (keymap-set eshell-mode-map "M-b" #'backward-sexp)
                              (keymap-set eshell-mode-map "<clear>" #'eshell-kill-input)
                              (keymap-set eshell-mode-map "M-f" #'cc/next-sexp)
                              (keymap-set eshell-mode-map "C-<left>" #'backward-sexp)
                              (keymap-set eshell-mode-map "C-<right>" #'cc/next-sexp)
                              ;; (keymap-set eshell-mode-map "C-<up>" #'backward-up-list)
                              ;; (keymap-set eshell-mode-map "C-<down>" #'down-list)
                              (setenv "NO_COLOR" "1")
                              (setenv "CLICOLOR" "0")))


(transient-define-prefix casual-eshell-tmenu ()
  "Transient menu for Eshell."
  ["Casual Eshell"
   ["Prompt"
    ("p" "Previous" eshell-previous-prompt :transient t)
    ("n" "Next" eshell-next-prompt :transient t)]

   ["Argument"
    ("b" "Backward" eshell-backward-argument :transient t)
    ("f" "Forward" eshell-forward-argument :transient t)
    ("y" "Repeat" eshell-repeat-argument :transient t)]

   ["Output"
    ("s" "Show" eshell-show-output)
    ("S" "Show Max" eshell-show-maximum-output)
    ("m" "Mark" eshell-mark-output)
    ("D" "Delete" eshell-delete-output)]

   ["Input"
    ("B" "Insert Buffer…" eshell-insert-buffer-name)
    ("k" "Kill Input" eshell-kill-input)
    ("h" "History" eshell-list-history)]

   ["Misc"
    ("d" "Dired" dired-jump-other-window)
    ("J" "Bookmark Jump…" bookmark-jump)
    ("g" "Magit" magit-status :if casual-editkit-version-controlled-p)]]

  [:class transient-row
   (casual-lib-quit-one)
   ("RET" "Dismiss" transient-quit-all)
   (casual-lib-quit-all)])


(keymap-set eshell-mode-map "C-o" #'casual-eshell-tmenu)


(provide 'cc-eshell-mode)
;;; cc-eshell-mode.el ends here
