;;; restlib.el --- Utility library for REST clients  -*- lexical-binding: t; -*-

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

;;

;;; Code:
(require 'url)
(require 'url-http)
(require 'map)

(defvar url-http-end-of-headers)


;;; Network

(defun restlib-fetch-json (url)
  "Synchronous fetch URL with expected JSON response.

The result is the JSON response deserialized into a `hash-table', with
JSON null values converted to nil."
  (let ((data-buffer (url-retrieve-synchronously url)))
    (if (not data-buffer)
        (error "Failed to fetch data from %s" url)
      (unwind-protect
          (with-current-buffer data-buffer
            ;; Move point past the HTTP metadata headers
            (goto-char url-http-end-of-headers)
            ;; Parse the remaining JSON buffer into a hash-table
            (json-parse-buffer :object-type 'hash-table
                               :null-object nil))
        ;; Always kill the downloaded network buffer to prevent memory leaks
        (kill-buffer data-buffer)))))


;;; URL Components

(defun restlib-url-add-query-items (url items &optional obj-result)
  "Add query ITEMS to URL and return string.

- URL: string or URL object
- ITEMS: list of lists as specified for `url-build-query-string'
- OBJ-RESULT: if non-nil then return URL object

If URL has an existing query fragment, then ITEMS will be naively
appended to it, with no regard for duplicate keys."

  (let* ((url (restlib-url-parse url))
         (old-items (restlib-url-query-items url))
         (items (if old-items
                    (append old-items items)
                  items))
         (query (url-build-query-string items))
         (url (restlib-url-remove-query url t))
         (filename (url-filename url))
         (new-filename (if filename
                           (format "%s?%s" filename query)
                         (format "?%s" query))))
    (setf (url-filename url) new-filename)

    (if obj-result
        url
      (url-recreate-url url))))

(defun restlib-url-parse (url)
  "Convenience function to return a parsed object given URL.

- URL: string or URL object

Will only parse if URL is a string."
  (if (stringp url)
      (url-generic-parse-url url)
    url))

(defun restlib-url-filename (url)
  "Get URL filename.

- URL: string or URL object"
  (let ((url (restlib-url-parse url)))
    (url-filename url)))

(defun restlib-url-path (url)
  "Get URL path.

- URL: string or URL object"
  (let* ((filename (restlib-url-filename url))
         (has-query (string-search "?" filename)))
    (if has-query
        (substring filename 0 has-query)
      filename)))

(defun restlib-url-query (url)
  "Get URL query.

- URL: string or URL object"
  (let* ((filename (restlib-url-filename url))
         (has-query (string-search "?" filename)))
    (if has-query
        (substring filename (+ has-query 1)))))

(defun restlib-url-query-items (url)
  "Extract query items from URL.

- URL: string or URL object"
  (let ((query (restlib-url-query url)))
    (if query
        (url-parse-query-string query))))

(defun restlib-url-remove-query (url &optional obj-result)
  "Remove query from URL.

- URL: string or URL object
- OBJ-RESULT: if non-nil then return URL object"

  (let* ((url (restlib-url-parse url))
         (filename (url-filename url))
         (index (string-search "?" filename)))
    (if index
        (setf (url-filename url) (substring filename 0 index)))

    (if obj-result
        url
      (url-recreate-url url))))


;;; JSON

(defun restlib-json-empty-string-to-nil (obj key)
  "Convert empty string value for KEY to nil in OBJ."
  (let ((value (map-elt obj key)))
    (if (and value (stringp value) (string-equal value ""))
        (map-put! obj key nil)
      (map-put! obj key value))))


(provide 'restlib)
;;; restlib.el ends here
