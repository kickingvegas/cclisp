;;; cc-emacs-lisp-mode.el --- Elisp Customization -*- lexical-binding: t; -*-

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

(require 'elisp-mode)
(require 'paredit)
(require 'flycheck)
(require 'cclisp)

;;; Code:

(add-hook 'emacs-lisp-mode #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode #'flycheck-mode)

(keymap-set emacs-lisp-mode-map "M-[" #'backward-sexp)
(keymap-set emacs-lisp-mode-map "M-]" #'forward-sexp)
(keymap-set emacs-lisp-mode-map "<f6>" #'cc/describe-function-point-is-in)
(keymap-set emacs-lisp-mode-map "M-j" #'fill-paragraph)
(keymap-set emacs-lisp-mode-map "M-n" #'cc/browse-forward-sexp)
(keymap-set emacs-lisp-mode-map "M-p" #'cc/browse-backward-sexp)

(provide 'cc-emacs-lisp-mode)
;;; cc-emacs-lisp-mode.el ends here
