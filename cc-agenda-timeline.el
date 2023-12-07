;;; cc-agenda-timeline.el --- Org Agenda Timeline    -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Charles Choi

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

;; Visualize Org Agenda as a horizontal timeline plot. Implementation is done
;; via PlantUML Gantt chart. Default output is SVG.

;;; Code:

(require 'org)

(defun cc/at/closed-agenda-element-p (e)
  (let ((closed (assoc-default "CLOSED" e))
        (timestamp (assoc-default "TIMESTAMP" e))
        (scheduled (assoc-default "SCHEDULED" e))
        (deadline (assoc-default "DEADLINE" e))                    
        )

    (if (or closed (not (or timestamp scheduled deadline)))
        t
      nil)))
    
(defun cc/test-timestamp (e)
  (let ((timestamp-buf (assoc-default "TIMESTAMP" e))
        (scheduled-buf (assoc-default "SCHEDULED" e))
        (deadline-buf (assoc-default "DEADLINE" e))
        (timestamp nil)
        (scheduled nil)
        (deadline nil))
    ;; TODO: handle possible case where timestamp and/or scheduled are repeating
    ;; TODO: handle date and time intervals "<2024-02-05 Mon>--<2024-02-12 Mon>"
    ;; (string-match "--" "<2024-02-05 Mon>--<2024-02-12 Mon>")
    
    (cond (timestamp-buf
           (if (stringp (org-get-repeat timestamp-buf))
               (setq timestamp (cc/at/next-time-from-repeating-org-timestamp timestamp-buf))
             (setq timestamp (time-convert (org-read-date nil t timestamp-buf) 'list)))
           ;; now timestamp is a Lisp timesstamp 
           (cc/timestamp-filter timestamp 2 45))

          (scheduled-buf
           (if (stringp (org-get-repeat scheduled-buf))
               (setq scheduled (cc/at/next-time-from-repeating-org-timestamp scheduled-buf))
             (setq scheduled (time-convert (org-read-date nil t scheduled-buf) 'list)))
           ;; now timestamp is a Lisp timesstamp 
           (cc/timestamp-filter scheduled 2 45))

          (deadline-buf
           (if (stringp (org-get-repeat deadline-buf))
               (setq deadline (cc/at/next-time-from-repeating-org-timestamp deadline-buf))
             (setq deadline (time-convert (org-read-date nil t deadline-buf) 'list)))
           ;; now timestamp is a Lisp timesstamp 
           (cc/timestamp-filter deadline 2 45))
          
          (t
           nil))))

;; this is good
(defun cc/at/next-time-from-repeating-org-timestamp (timestamp)
  "Next Lisp timestamp if Org string TIMESTAMP is repeating.

If TIMESTAMP is repeating then return the next Lisp timestamp (ticks . hz),
otherwise nil."
  (let* ((repeat-value (org-get-repeat timestamp))
         (from-string (concat " " repeat-value))
         (amended-timestamp (concat repeat-value
                                    " "
                                    (string-replace from-string "" timestamp))))
    (if (not repeat-value)
        (error "timestamp `%s' does not have a repeat value." timestamp))
    (time-convert (org-read-date nil t amended-timestamp) 'list)))



(defun cc/timestamp-filter (timestamp start end) 
  (let ((start-timestamp (time-subtract (current-time) (* 60 60 24 start)))
        (end-timestamp (time-add (current-time) (* 60 60 24 end))))
    (if (and (time-less-p start-timestamp timestamp) (time-less-p timestamp end-timestamp))
        timestamp
      nil)))


(defun cc/agenda-timeline-items (match scope)
  (interactive)
  (let* ((agenda-items (org-map-entries 'org-entry-properties match scope))
         (active-agenda-items (seq-remove 'cc/at/closed-agenda-element-p agenda-items))
         (timestamped-items (seq-filter 'cc/test-timestamp active-agenda-items))
         (items (mapcar (lambda (x) (list
                              (assoc-default "ITEM" x)
                              (cc/test-timestamp x)))
                        timestamped-items))
         (items-with-diary (append items (cc/extract-diary-items 40)))
         (sorted-items (sort items-with-diary (lambda (x y) (time-less-p (nth 1 x) (nth 1 y)))))
         (results (list)))
    
    (mapc (lambda (x)
              (push (format "[%s] happens %s" (cc/scrub-item-string (nth 0 x)) (format-time-string "%Y-%m-%d" (nth 1 x))) results)) sorted-items)

    (reverse results)))

(defun cc/agenda-timeline (match scope)
  (interactive)
  (let* ((headers (list))
         (footers (list))
         (today (format-time-string "%Y-%m-%d" (time-subtract (current-time) (* 60 60 24 2)))))

    (push "@startgantt" headers)
    (push "printscale daily" headers)
    (push "hide footbox" headers)
    (push "saturday is colored wheat" headers)
    (push "sunday is colored wheat" headers)
    (push "today is colored in Orange" headers)
    (push "skinparam backgroundColor #EEEEEE" headers)
    (push (format "Project starts %s" today) headers)
    
    (push "@endgantt" footers)

    (mapconcat 'identity 
               (append (reverse headers)
                       (cc/agenda-timeline-items match scope)
                       (reverse footers))
               "\n")))

(defun cc/extract-diary-items (days)
  (let ((diary-entries (diary-list-entries (calendar-current-date) days t))
         )
   (mapcar 'cc/diary-info diary-entries)))

(defun cc/diary-info (e)
  (let* ((date-list (nth 0 e))
         (item (nth 1 e))
         (month (nth 0 date-list))
         (day (nth 1 date-list))
         (year (nth 2 date-list))
         (datestamp (format "%4d-%02d-%02d" year month day))
         (results (list)))
    
    (push (time-convert (date-to-time datestamp) 'list) results)
    (push item results)
    results
    ))

(defun cc/scrub-item-string (item)
  (string-trim
   (replace-regexp-in-string "\\[\\([[:digit:]]*\\)/\\([[:digit:]]*\\)\\]" "(\\1/\\2)"
    (replace-regexp-in-string "<[[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}.*>" "" item))))

;; (cc/scrub-item-string "한글 <2023-10-23 Mon 10:00-11:00 +1d> [2/4]")

;;(kill-new (cc/agenda-timeline t 'agenda))


;;(org-map-entries 'org-entry-properties "+dj" 'agenda)

;; (kill-new (pp (org-map-entries 'org-entry-properties t 'agenda)))

;; (org-get-repeat "<2023-08-31 Thu 22:00 +1w>")

;; (org-time-string-to-absolute "<2023-08-31 Thu 22:00 +1w>")

;; (defun cc/org-timestamp-process (timestamp)
;;   )

;; TODO: This right here
;; (org-read-date nil nil "<2023-08-31 Thu 22:00 +1w>")

;; (kill-new (pp (diary-list-entries (calendar-current-date) 40 t)))

;; (kill-new (pp (seq-filter 'cc/test-timestamp (seq-remove 'cc/at/closed-agenda-element-p (org-map-entries 'org-entry-properties "+dj" 'agenda)))))

(provide 'cc-agenda-timeline)
;;; cc-agenda-timeline.el ends here
