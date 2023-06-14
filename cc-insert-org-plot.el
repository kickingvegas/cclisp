;;; cc-insert-org-plot.el --- Functions supporting Org Plot configuration

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

(defvar cc/insert-org-plot-menu (make-sparse-keymap "Insert Org Plot")
  "Keymap for Org plot submenu.")

(cc/add-first-context-menu-item cc/insert-org-plot-menu
                                cc/org-plot-insert-lines-plot
                                "Lines - GUI"
                                "Insert lines, linespoints, or impulses \
style chart for GUI interaction")

(cc/add-context-menu-item cc/insert-org-plot-menu
                      cc/org-plot-insert-lines-timestamp-plot
                      "Lines Timestamp - GUI"
                      "Insert lines, linespoints, or impulses style \
chart with timestamps for GUI interaction")

(cc/add-context-menu-item cc/insert-org-plot-menu
                      cc/org-plot-insert-histogram-plot
                      "Histogram - GUI"
                      "Insert histogram (column) style chart for GUI interaction")

(cc/context-menu-item-separator cc/insert-org-plot-menu
                                org-plot-menu-single-separator)


(cc/add-context-menu-item cc/insert-org-plot-menu
                      cc/org-plot-insert-lines-plot-image
                      "Lines - Image"
                      "Insert lines, linespoints, or impulses style chart for \
image creation")

(cc/add-context-menu-item cc/insert-org-plot-menu
                      cc/org-plot-insert-lines-timestamp-plot-image
                      "Lines Timestamp - Image"
                      "Insert lines, linespoints, or impulses style chart \
with timestamps for image creation")

(cc/add-context-menu-item cc/insert-org-plot-menu
                      cc/org-plot-insert-histogram-plot-image
                      "Histogram - Image"
                      "Insert histogram (column) style chart for image creation")

(provide 'cc-insert-org-plot)

;;; cc-insert-org-plot.el ends here
