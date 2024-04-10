;;; cc-org-smart-quotes.el --- CC Org Smart Quotes Config  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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
(require 'org)
(require 'ox)

(defun cc/reconfig-org-smart-quotes-lang (lang)
  "Reconfigure Org smart quotes to use utf-8 per LANG."
  (let* ((db-entry (assoc-string lang org-export-smart-quotes-alist))
         (utf8-primary-opening (plist-get (assoc-default 'primary-opening db-entry) :utf-8))
         (utf8-primary-closing (plist-get (assoc-default 'primary-closing db-entry) :utf-8))
         (utf8-secondary-opening (plist-get (assoc-default 'secondary-opening db-entry) :utf-8))
         (utf8-secondary-closing (plist-get (assoc-default 'secondary-closing db-entry) :utf-8))
         (utf8-apostrophe (plist-get (assoc-default 'apostrophe db-entry) :utf-8))
         )

    (setf (plist-get
           (assoc-default 'primary-opening
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-primary-opening)

    (setf (plist-get
           (assoc-default 'primary-closing
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-primary-closing)

    (setf (plist-get
           (assoc-default 'secondary-opening
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-secondary-opening)

    (setf (plist-get
           (assoc-default 'secondary-closing
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-secondary-closing)

    (setf (plist-get
           (assoc-default 'apostrophe
                          (assoc-string lang org-export-smart-quotes-alist))
           :html)
          utf8-apostrophe)))


(provide 'cc-org-smart-quotes)
;;; cc-org-smart-quotes.el ends here
