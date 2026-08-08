;;; wttr.el --- wttr interface                       -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Choi

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

;; Get weather conditions for a location from `https://wttr.in'.
;; Provides the command `wttr'.

;;

;;; Code:
(require 'restlib)

(defvar url-http-end-of-headers)        ; needed for clean byte-compile

(defun wttr--request-url (location)
  "Construct wttr.in URL with LOCATION."
  (restlib-url-add-query-items (format "%s/%s"
                                  "https://wttr.in"
                                  (string-replace " " "+" location))
                          '(("0")
                            ("format" "j1"))))

(defun wttr--get-first (dict key)
  "Get first object DICT with KEY."
  (if (map-contains-key dict key)
      (let ((obj (map-elt dict key)))
        (if (and (vectorp obj)
                 (> (length obj) 0))
            (seq-elt obj 0)))
    (error "Unable to extract %s from %s" key dict)))

(defun wttr--report-message (jsondb)
  "Generate weather report message from JSONDB."
  (let* ((area-buflist ())
         (nearest-area
          (wttr--get-first jsondb "nearest_area"))
         (area-name
          (map-elt (wttr--get-first nearest-area "areaName") "value"))
         (region
          (map-elt (wttr--get-first nearest-area "region") "value"))
         (country
          (map-elt (wttr--get-first nearest-area "country") "value"))

         (current-condition (wttr--get-first jsondb "current_condition"))
         (temp_c (map-elt current-condition "temp_C"))
         (temp_f (map-elt current-condition "temp_F"))

         (weather-description
          (map-elt
           (wttr--get-first current-condition "weatherDesc") "value")))

    (mapc (lambda (x)
            (if (and x (not (string-equal x "")))
                (push x area-buflist)))
          (list area-name region country))

    (format "%s: %s°C, %s°F %s"
            (string-join (reverse area-buflist) ", ")
            temp_c
            temp_f
            weather-description)))

(defun wttr (&optional location)
  "Show weather conditions for LOCATION from `https://wttr.in' in mini-buffer.

Result is also stored in `kill-ring'."
  (interactive "sWhere (default: local): ")

  (condition-case err
      (let* ((location (cond
                        ((or (not location)
                             (and (stringp location)
                                  (string-equal location "")
                                  calendar-latitude
                                  calendar-longitude))
                         (format "%s,%s" calendar-latitude calendar-longitude))
                        (t
                         location)))
             (url (wttr--request-url location))
             (jsondb (restlib-fetch-json url))
             (msg (wttr--report-message jsondb)))
        (kill-new msg)
        (message "%s" msg))
    (error (message "ERROR: %s" (cdr err)))))


(provide 'wttr)
;;; wttr.el ends here
