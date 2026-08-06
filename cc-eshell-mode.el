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
(require 'em-unix)
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

(defun cc/eshell-top-marker (path)
  "Top marker for prompt given PATH."
  ;; Possible 􀡥􀢹􀟛􀈕􀪯􁕔􁟬􀨺

  (let* ((sfsymbols-test (and (display-graphic-p) (eq system-type 'darwin)))
         (ssh-test (string-search "/ssh:" path)))

    (if sfsymbols-test
        (cond
         ((and ssh-test (= ssh-test 0)) "\n┏􀧘")
         (t "\n┏􀟛"))
      "\n┏━")))

(defun cc/prompt-function ()
  "Eshell prompt function for Charles Choi."

  (let* ((uname (user-login-name))
         (sysname (system-name))
         (user-at-sys (format "%s@%s" uname sysname))
         (path (eshell/pwd))
         (curdir (propertize (casual-eshell-tilde-path path)
                             'face `(:foreground "orange red")))
         (branch-name (eshell-git-prompt--branch-name))
         (git-branch (if branch-name
                         (format " (%s)" branch-name)
                       ""))

         (prompt-symbol (if (= (user-uid) 0) "# " "$ "))
         (top-marker (cc/eshell-top-marker path))
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


(defun eshell/bufcat (&rest args)
  "Support cat on a buffer specified in ARGS.

Taken from
URL `https://emacs.stackexchange.com/questions/54766/piping-contents-of-buffer-into-eshell-command'"
  (if (bufferp (car args))
      (with-current-buffer (car args)
        (buffer-string))
    (apply #'eshell/cat args)))

(provide 'cc-eshell-mode)
;;; cc-eshell-mode.el ends here
