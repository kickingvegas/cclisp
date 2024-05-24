;;; cc-view-mode.el -*- lexical-binding: t; -*-

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

;;; Code:

(require 'view)
(require 'cclisp)
(require 'org)
(require 'markdown-mode)
(require 'sgml-mode)
(require 'python)
(require 'elisp-mode)
(require 'make-mode)
(require 'cc-cmds)

(add-hook 'view-mode-hook 'hl-line-mode)

(defun cc/view-exit ()
  "Advice function to disable highlighting upon exiting view-mode."
  (if (not (derived-mode-p 'prog-mode))
      (hl-line-mode -1)))

(advice-add 'View-exit :after #'cc/view-exit)

(add-hook
 'view-mode-hook
 (lambda ()
   (cond ((derived-mode-p 'org-mode)
          (keymap-set view-mode-map "j" #'cc/browse-forward-paragraph)
          (keymap-set view-mode-map "k" #'cc/browse-backward-paragraph)
          (keymap-set view-mode-map "p" #'org-previous-visible-heading)
	  (keymap-set view-mode-map "n" #'org-next-visible-heading))
         ((derived-mode-p 'markdown-mode)
          (keymap-set view-mode-map "j" #'cc/browse-forward-paragraph)
          (keymap-set view-mode-map "k" #'cc/browse-backward-paragraph)
          (keymap-set view-mode-map "p" #'markdown-outline-previous)
          (keymap-set view-mode-map "n" #'markdown-outline-next))
         ((derived-mode-p 'html-mode)
          (keymap-set view-mode-map "p" #'sgml-skip-tag-backward)
	  (keymap-set view-mode-map "n" #'sgml-skip-tag-forward))
         ((derived-mode-p 'python-mode)
          (keymap-set view-mode-map "p" #'python-nav-backward-block)
	  (keymap-set view-mode-map "n" #'python-nav-forward-block))
         ((derived-mode-p 'emacs-lisp-mode)
          (keymap-set view-mode-map "p" #'backward-sexp)
	  (keymap-set view-mode-map "n" #'forward-sexp))
         ((derived-mode-p 'makefile-mode)
          (keymap-set view-mode-map "p" #'makefile-previous-dependency)
	  (keymap-set view-mode-map "n" #'makefile-next-dependency))
         ((derived-mode-p 'c-mode)
          (keymap-set view-mode-map "p" #'c-beginning-of-defun)
          (keymap-set view-mode-map "n" #'c-end-of-defun))
         (t
          (keymap-set view-mode-map "p" #'scroll-down-command)
          (keymap-set view-mode-map "n" #'scroll-up-command)))))

(provide 'cc-view-mode)
;;; cc-view-mode.el ends here
