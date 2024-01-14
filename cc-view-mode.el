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
          (define-key view-mode-map (kbd "p") 'org-previous-visible-heading)
	  (define-key view-mode-map (kbd "n") 'org-next-visible-heading))
         ((derived-mode-p 'markdown-mode)
          (define-key view-mode-map (kbd "p") 'markdown-outline-previous)
          (define-key view-mode-map (kbd "n") 'markdown-outline-next))
         ((derived-mode-p 'html-mode)
          (define-key view-mode-map (kbd "p") 'sgml-skip-tag-backward)
	  (define-key view-mode-map (kbd "n") 'sgml-skip-tag-forward))
         ((derived-mode-p 'python-mode)
          (define-key view-mode-map (kbd "p") 'python-nav-backward-block)
	  (define-key view-mode-map (kbd "n") 'python-nav-forward-block))
         ((derived-mode-p 'emacs-lisp-mode)
          (define-key view-mode-map (kbd "p") 'backward-sexp)
	  (define-key view-mode-map (kbd "n") 'forward-sexp))
         ((derived-mode-p 'makefile-mode)
          (define-key view-mode-map (kbd "p") 'makefile-previous-dependency)
	  (define-key view-mode-map (kbd "n") 'makefile-next-dependency))
         ((derived-mode-p 'c-mode)
          (define-key view-mode-map (kbd "p") 'c-beginning-of-defun)
          (define-key view-mode-map (kbd "n") 'c-end-of-defun))
         (t
          (define-key view-mode-map (kbd "p") 'scroll-down-command)
          (define-key view-mode-map (kbd "n") 'scroll-up-command)))))

(provide 'cc-view-mode)
;;; cc-view-mode.el ends here
