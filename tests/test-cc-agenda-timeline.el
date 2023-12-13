(require 'ert)
(require 'cc-agenda-timeline)

(ert-deftest test-cc/at/closed-agenda-element-p ()
  (let ()
    (should (cc/at/closed-agenda-element-p '(("CLOSED" . "<2023-12-03 Sun 16:20>"))))
    (should-not (cc/at/closed-agenda-element-p '(("TIMESTAMP" . "<2023-12-03 Sun 16:20>"))))
    (should-not (cc/at/closed-agenda-element-p '(("SCHEDULED" . "<2023-12-03 Sun 16:20>"))))
    (should-not (cc/at/closed-agenda-element-p '(("DEADLINE" . "<2023-12-03 Sun 16:20>"))))
    (should-not (cc/at/closed-agenda-element-p '(("DEADLINE" . "<2023-12-03 Sun 16:20>")
                                                 ("SCHEDULED" . "<2023-12-03 Sun 16:20>")
                                                 ("TIMESTAMP" . "<2023-12-03 Sun 16:20>"))))

    (should (cc/at/closed-agenda-element-p '(("CLOSED" . "<2023-12-03 Sun 16:20>")
                                             ("DEADLINE" . "<2023-12-03 Sun 16:20>")
                                             ("SCHEDULED" . "<2023-12-03 Sun 16:20>")
                                             ("TIMESTAMP" . "<2023-12-03 Sun 16:20>"))))))

;; (ert-deftest test-cc/test-timestamp ()
;;   (let ()
    
;;     ))


(ert-deftest test-cc/timestamp-filter ()
  (let* (
         (now (current-time))
         (now-string (format-time-string "<%Y-%m-%d %a %H:%M>" now))
         (filter-start 2)
         (filter-end 5)
         (prior-timestamp (time-subtract now (* 60 60 24 (+ filter-start 1))))
         (post-timestamp (time-add now (* 60 60 24 (+ filter-end 1))))
         )
    (should (cc/timestamp-filter now filter-start filter-end))
    (should-not (cc/timestamp-filter prior-timestamp filter-start filter-end))
    (should-not (cc/timestamp-filter post-timestamp filter-start filter-end))))

    
