;;; cc-insert-org-plot.el --- Functions supporting Org Plot configuration

;;; Commentary:
;; 

;;; Code:

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

(defvar cc/insert-org-plot-menu (make-sparse-keymap "Insert Org Plot"))

(define-key cc/insert-org-plot-menu [lines-interactive]
  '(menu-item "Lines - GUI" cc/org-plot-insert-lines-plot
              :help "Insert lines, linespoints, or impulses style chart for GUI interaction"))

(define-key-after cc/insert-org-plot-menu [lines-timestamp-interactive]
  '(menu-item "Lines Timestamp - GUI" cc/org-plot-insert-lines-timestamp-plot
              :help "Insert lines, linespoints, or impulses style chart with timestamps for GUI interaction"))

(define-key-after cc/insert-org-plot-menu [columns-interactive]
  '(menu-item "Histogram - GUI" cc/org-plot-insert-histogram-plot
              :help "Insert histogram (column) style chart for GUI interaction"))

(define-key-after cc/insert-org-plot-menu [org-plot-menu-single-separator]
  '(menu-item "--single-line"))

(define-key-after cc/insert-org-plot-menu [lines-image]
  '(menu-item "Lines - Image" cc/org-plot-insert-lines-plot-image
              :help "Insert lines, linespoints, or impulses style chart for image creation"))

(define-key-after cc/insert-org-plot-menu [lines-timestamp-image]
  '(menu-item "Lines Timestamp - Image" cc/org-plot-insert-lines-timestamp-plot-image
              :help "Insert lines, linespoints, or impulses style chart with timestamps for image creation"))

(define-key-after cc/insert-org-plot-menu [columns-image]
  '(menu-item "Histogram - Image" cc/org-plot-insert-histogram-plot-image
              :help "Insert histogram (column) style chart for image creation"))

(provide 'cc-insert-org-plot)

;;; cc-insert-org-plot.el ends here
