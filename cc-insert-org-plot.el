;;; cc-insert-org-plot.el --- Functions supporting Org Plot configuration -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cc-context-menu-macros)

(defmacro cc/insert-org-plot-snippet (snippet-name)
  `(lambda ()
     (if (org-at-table-p)
         (progn
           (org-backward-paragraph)
           (yas-expand-snippet (yas-lookup-snippet ,snippet-name)))
       (message "Error: not in org-table"))))

(defun cc/org-plot-insert-snippet (name)
  "Insert NAME snippet at top of the table.

If the point is not within an Org table then an error message
will be displayed."
  (if (org-at-table-p)
      (progn
        (org-backward-paragraph)
        (yas-expand-snippet (yas-lookup-snippet name)))
    (message "Error: not in org-table")))

(defun cc/org-plot-insert-lines-plot ()
  "Invoke snippet named \"org-plot lines\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot lines"))

(defun cc/org-plot-insert-lines-timestamp-plot ()
  "Invoke snippet named \"org-plot lines timestamp\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot lines timestamp"))

(defun cc/org-plot-insert-histogram-plot ()
  "Invoke snippet named \"org-plot histogram\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot histogram"))

(defun cc/org-plot-insert-lines-plot-image ()
  "Invoke snippet named \"org-plot lines image\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot lines image"))

(defun cc/org-plot-insert-lines-timestamp-plot-image ()
  "Invoke snippet named \"org-plot lines timestamp image\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot lines timestamp image"))

(defun cc/org-plot-insert-histogram-plot-image ()
  "Invoke snippet named \"org-plot histogram image\"."
  (interactive)
  (cc/org-plot-insert-snippet "org-plot histogram image"))

(easy-menu-define cc/insert-org-plot-menu nil
  "Kaymap for Org Plot submenu"
  '("Org Plot"
    ["Lines - GUI"
     cc/org-plot-insert-lines-plot
     :help "Insert lines, linespoints, or impulses \
style chart for GUI interaction"]

    ["Lines Timestamp - GUI"
     cc/org-plot-insert-lines-timestamp-plot
     :help "Insert lines, linespoints, or impulses style \
chart with timestamps for GUI interaction"]

    ["Histogram - GUI"
     cc/org-plot-insert-histogram-plot
     :help "Insert histogram (column) style chart for GUI interaction"]

    "---"

    ["Lines - Image"
     cc/org-plot-insert-lines-plot-image
     :help "Insert lines, linespoints, or impulses style chart for \
image creation"]

    ["Lines Timestamp - Image"
     cc/org-plot-insert-lines-timestamp-plot-image
     :help "Insert lines, linespoints, or impulses style chart \
with timestamps for image creation"]


    ["Histogram - Image"
     cc/org-plot-insert-histogram-plot-image
     :help "Insert histogram (column) style chart for image creation"]))

(provide 'cc-insert-org-plot)
;;; cc-insert-org-plot.el ends here
