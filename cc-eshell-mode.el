;;; cc-eshell-mode.el --- eshell customization -*- lexical-binding: t; -*-

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

;;; Code:
(require 'eshell)
(require 'esh-mode)
(require 'em-hist)
(require 'company)
(require 'hl-line)
(require 'helm-eshell)
(require 'eshell-git-prompt)
(require 'cclisp)
(require 'casual-eshell)
(require 'with-editor)
(require 'goto-addr)

(defvar eshell-mode-map)
(defvar eshell-visual-options)
(defvar eshell-visual-commands)
(defvar eshell-visual-subcommands)
(declare-function eshell/pwd "pwd" ())

(defun cc/prompt-function ()
  "Eshell prompt function for Charles Choi."

  (let* ((uname (user-login-name))
         (sysname (system-name))
         (user-at-sys (format "%s@%s" uname sysname))
         (curdir (propertize (casual-eshell-tilde-path (eshell/pwd))
                             'face `(:foreground "orange red")))
         (branch-name (eshell-git-prompt--branch-name))
         (git-branch (if branch-name
                         (format " (%s)" branch-name)
                       ""))

         (prompt-symbol (if (= (user-uid) 0) "# " "$ "))
         (top-marker "\n┏━")
         (bottom-marker "\n┗━━"))

    (format "%s %s:%s%s%s%s"
            top-marker
            user-at-sys
            curdir
            git-branch
            bottom-marker
            prompt-symbol)))

(setopt eshell-prompt-function #'cc/prompt-function)
;;(setopt eshell-banner-message (format "Eshell ⌨️\n%s" (sunrise-sunset)))

;;(add-hook 'eshell-mode-hook #'company-mode)
(add-hook 'eshell-mode-hook #'hl-line-mode)
(add-hook 'eshell-mode-hook #'with-editor-export-editor)
(add-hook 'eshell-mode-hook #'goto-address-mode)
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

(keymap-set eshell-mode-map "C-o" #'casual-eshell-tmenu)

(provide 'cc-eshell-mode)
;;; cc-eshell-mode.el ends here
