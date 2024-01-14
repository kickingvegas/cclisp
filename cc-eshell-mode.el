;;; cc-eshell-mode.el --- eshell customization -*- lexical-binding: t; -*-

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

(require 'eshell)
;;; Code:

(defvar eshell-mode-map)
(defvar eshell-visual-options)
(defvar eshell-visual-commands)
(defvar eshell-visual-subcommands)
(declare-function eshell/pwd "pwd" ())

(setq eshell-prompt-regexp "┗━━ \\$ "
      eshell-prompt-function
      (lambda nil
        (concat "\n┏━ "
                (user-login-name) "@" (system-name) ":"
                (propertize (if (string= (eshell/pwd) (getenv "HOME"))
                                "~"
                              (replace-regexp-in-string
                               (concat "^" (getenv "HOME")) "~" (eshell/pwd)))
                            'face `(:foreground "orange red"))
                "\n┗━━ "
                (if (= (user-uid) 0) "# " "$ "))))

(add-hook 'eshell-mode-hook 'company-mode)
(add-hook 'eshell-mode-hook (lambda ()
			      (define-key eshell-mode-map (kbd "<tab>") 'company-complete)
			      (define-key eshell-mode-map (kbd "C-r") 'helm-eshell-history)
			      (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
			      (add-to-list 'eshell-visual-options '("gh" "help"))
			      (add-to-list 'eshell-visual-options '("swift" "repl"))
			      (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
                              (push "gdu-go" eshell-visual-commands)
                              (push "gh" eshell-visual-commands)))

(provide 'cc-eshell-mode)
;;; cc-eshell-mode.el ends here
