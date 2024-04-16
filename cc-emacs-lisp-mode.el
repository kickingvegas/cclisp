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

(define-key emacs-lisp-mode-map (kbd "M-[") #'backward-sexp)
(define-key emacs-lisp-mode-map (kbd "M-]") #'forward-sexp)
(define-key emacs-lisp-mode-map (kbd "C-h ;") #'cc/describe-function-point-is-in)
(define-key emacs-lisp-mode-map (kbd "M-j") #'cc/repunctuate-and-fill-paragraph)

(provide 'cc-emacs-lisp-mode)
;;; cc-emacs-lisp-mode.el ends here
