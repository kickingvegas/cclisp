;;; cc-bmark.el --- Bookmark Extensions              -*- lexical-binding: t; -*-

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

(require 'bookmark)
(require 'url-parse)
(require 'browse-url)
(require 'org-element)
(require 'org-element-ast)



;; bookmark-bmenu setup

(defun cc-bookmark-bmenu-mouse-mark ()
  "Mark bookmark with mouse."
  (interactive)
  (mouse-set-point last-input-event)
  (save-excursion
    (forward-line 0)
    (if (looking-at-p ">")              ; oof this check is fragile
        (bookmark-bmenu-unmark)
      (bookmark-bmenu-mark)
      (forward-line -1))))

(defun cc-bookmark-bmenu-setup ()
  "Setup function for bookmarks list editor."
  (setq-local mouse-1-click-follows-link 'double)
  (keymap-set bookmark-bmenu-mode-map "M-<mouse-1>" #'cc-bookmark-bmenu-mouse-mark))


(add-hook 'bookmark-bmenu-mode-hook #'cc-bookmark-bmenu-setup)


;; URL Bookmark Support

(defun cc-bmark-jump (bookmark)
  "Default BOOKMARK handler for bmark."
  (let* ((location (bookmark-prop-get bookmark 'location)))
    (browse-url location)))

(put 'cc-bmark-jump 'bookmark-handler-type "webmark")


(defun cc-bmark--store-link (location &optional name)
  "Store link (LOCATION, NAME) in bookmarks."
  (let* ((loc location)
         (name (if name
                   name
                 (let ((url (url-generic-parse-url loc)))
                   (url-host url))))

         (properties `((location . ,loc)
                       (handler . cc-bmark-jump))))
    (bookmark-store name properties nil)))


(defun cc-bmark-add-org-link ()
  "Add Org link to bookmarks."
  (interactive nil org-mode)

  (let ((link (org-element-context)))
    (if (not (eq (org-element-type link) 'link))
        (error "Not on Org link")
      (let* ((raw (org-element-property :raw-link link))
             (desc-start (org-element-property :contents-begin link))
             (desc-end (org-element-property :contents-end link))
             (title (if (and desc-start desc-end)
                        (buffer-substring-no-properties desc-start desc-end))))

        (cc-bmark--store-link raw title)
        (message "Added (%s, %s) to bookmarks" title raw)))))

(defun cc-bmark-add (arg)
  "Add link with ARG interactively to bookmarks."
  (interactive "sBookmark Name: ")
  (let* ((name arg)
         (url (read-buffer "URL: ")))
    (cc-bmark--store-link url name)
    (message "Added (%s, %s) to bookmarks" name url)))

(provide 'cc-bmark)
;;; cc-bmark.el ends here
