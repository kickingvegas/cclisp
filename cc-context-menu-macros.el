;;; cc-context-menu-macros.el --- Context Menu Macros -*- lexical-binding: t; -*-

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
(require 'ol)

(defmacro cc/context-menu-item-separator (menu key)
  "Add single line separator to MENU with KEY."
  `(define-key-after ,menu [,key]
     '(menu-item "--single-line")))

(defmacro cc/add-context-menu-item (menu command label help)
  "Add COMMAND to MENU annotated with LABEL and property HELP."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-item-visible (menu command label help visible)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, VISIBLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :visible ,visible)))

(defmacro cc/add-context-menu-item-enable (menu command label help enable)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, ENABLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :enable ,enable)))

(defmacro cc/add-first-context-menu-item (menu command label help)
  "Add first COMMAND to MENU annotated with LABEL and HELP."
  `(define-key ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-submenu (menu submenu label)
  "Add SUBMENU to MENU annotated with LABEL.
SUBMENU is a keymap."
  `(define-key-after ,menu [,submenu]
     (list 'menu-item ,label ,submenu)))

(defun cc/context-menu-label (prefix &optional max extent)
  "Generate context menu label with region string prepended by PREFIX.

- MAX defines the truncation length of the region.
- EXTENT defines the length of the truncated string to show from start,
  end of region.

The truncation is done “Apple-style” using `cc/apple-style-truncate'."
  (let* ((start (region-beginning))
         (end (region-end))
         (rstring (buffer-substring-no-properties start end)))

    (catch 'cc/apple-style-truncate-exception
        (cc/apple-style-truncate rstring prefix max extent))))

(defun cc/apple-style-truncate (rstring prefix &optional max extent)
  "Apple-style truncate RSTRING prepended by PREFIX.

Implementation of Apple-style truncation labels.

- RSTRING is the source string (typically a region) to be truncated.
- PREFIX is a string to prepend the truncated string.
- MAX defines the truncation length.
- EXTENT defines the length of the truncated string to show from start,
  end of region.

This idea came from Scott Jenson as detailed in the URL
`https://www.linkedin.com/posts/scottjenson_one-of-my-earliest-ux-wins-was-for-mac-system-activity-7275265246053720064-Ozha'."
  (let* ((max (if (not max) 30 max))
         (extent (if (not extent) 12 extent))
         (rlist (string-split rstring "\n")))

    (unless (>= (- (/ max 2) 2) extent)
      (let ((msg (format
                  "ERROR: extent (%d) and max (%d) should \
conform to extent <= (max/2) - 2"
                  extent max)))
        (throw 'cc/apple-style-truncate-exception msg)))

    (if (> (length rlist) 1)
        (let* ((first (nth 0 rlist))
               (first (if (> (length first) max)
                          (substring first 0 extent)
                        first))
               (last (car (last rlist)))
               (last (if (> (length last) max)
                         (substring last (* -1 extent))
                       last))
               (last (string-trim-left last))
               (last (if (string-equal last "")
                         "␤"
                       last))
               (first (if (string-equal (string-trim first) "")
                         "␣"
                       first)))
          (format "%s “%s…%s”" prefix first last))

      (if (> (length rstring) max)
          (let* ((first (substring rstring 0 extent))
                 (last (string-trim-left (substring rstring (* -1 extent)))))
            (format "%s “%s…%s”" prefix first last))
        (format "%s “%s”" prefix rstring)))))

(defun cc/occur-selected-region ()
  "Occur selected region."
  (interactive)
  (let* ((start (region-beginning))
         (end (region-end))
         (regex (buffer-substring-no-properties start end)))
    (occur regex)))

(defun cc/context-menu-last-word-in-region (prefix)
  "Generate context menu label with last word in region prepended by PREFIX."
  (let*  ((start (region-beginning))
         (end (region-end))
         (buf (buffer-substring start end))
         (last-word (car (last (split-string buf " ")))))
    (concat prefix " “" last-word "”")))

(defun cc/org-stored-links-p ()
  "Predicate if `org-stored-links' is populated.
Return t if populated, nil otherwise."
  (if (> (length org-stored-links) 0)
      t
    nil))

(provide 'cc-context-menu-macros)
;;; cc-context-menu-macros.el ends here
