;;; cc-style-text-menu.el --- Style Text Menus -*- lexical-binding: t; -*-

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

(require 'cc-context-menu-macros)
(require 'markdown-mode)
(require 'org)

;; (defun cc/org-emphasize-reset ()
;;   ;; this won't work when org-hide-emphasis-markers is turned on.
;;   (interactive)
;;   (org-emphasize ?\s))

;;; Code:

(defun cc/emphasize-bold ()
  "Mark region bold for Org or Markdown modes."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?*))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-bold))
        (t nil)))

(defun cc/emphasize-italic ()
  "Mark region italic for Org or Markdown modes."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?/))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-italic))
        (t nil)))

(defun cc/emphasize-code ()
  "Mark region code for Org or Markdown modes."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?~))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-code))
        (t nil)))

(defun cc/emphasize-underline ()
  "Mark region underline for Org mode."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?_))
        (t nil)))

(defun cc/emphasize-verbatim ()
  "Mark region verbatim for Org mode."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?=))
        (t nil)))

(defun cc/emphasize-strike-through ()
  "Mark region strike-through for Org or Markdown modes."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?+))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-strike-through))
        (t nil)))

(defun cc/emphasize-remove ()
  "Remove marked region."
  (interactive)
  (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ? ))
        ((derived-mode-p 'markdown-mode)
         (message "unsupported."))
        (t nil)))

(defun cc/emphasize-dwim ()
  "DWIM emphasize text for Org or Markdown.

This command will appropriately style either a region or the text
the point is in depending on whether the current major mode is
Org or Markdown. Selection of the emphasis style is done by
mini-buffer command completion.

If no region is defined, then the text amount is considered to be
a balanced expression (sexp). A balanced expression is used as it
can cover most cases of applying the style to text that is
contiguous without spaces."
  (interactive)
  (let* ((styles (list "bold" "italic" "code"
                       "underline" "verbatim" "strike" "remove"))
         (choice (car (completing-read-multiple "Style: " styles))))
    (when (not (use-region-p))
      (beginning-of-thing 'sexp)
      (mark-sexp))
    (cond
     ((string= choice "bold") (cc/emphasize-bold))
     ((string= choice "italic") (cc/emphasize-italic))
     ((string= choice "code") (cc/emphasize-code))
     ((string= choice "verbatim") (cc/emphasize-verbatim))
     ((string= choice "underline") (cc/emphasize-underline))
     ((string= choice "strike") (cc/emphasize-strike-through))
     ((string= choice "remove")
      (if (derived-mode-p 'org-mode)
          (org-emphasize ? )
        (message "remove not supported for Markdown.")))
     (t (message "ERROR: undefined choice: %s" choice)))))

(easy-menu-define cc/emphasize-menu nil
  "Keymap for Emphasize Menu."
  '("Style"
    :visible (region-active-p)
    ["Bold" cc/emphasize-bold
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Bold selected region"]
    ["Italic" cc/emphasize-italic
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Italic selected region"]
    ["Code" cc/emphasize-code
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Code selected region"]
    ["Underline" cc/emphasize-underline
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)
     :help "Underline selected region"]
    ["Verbatim" cc/emphasize-verbatim
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)
     :help "Verbatim selected region"]
    ["Strike Through" cc/emphasize-strike-through
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Strike-through selected region"]))

(provide 'cc-style-text-menu)

;;; cc-style-text-menu.el ends here
