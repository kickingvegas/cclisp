;;; scrim-utils.el --- Scrim Utilities               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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

(require 'info)
(require 'url-util)

(defun scrim-copy-file-link (arg)
  "Copy Scrim file URL into the kill ring with prefix ARG.

Generate a Scrim file URL “scrim://open” for the current buffer
associated with a file.

By default the path uses the ‘~’ to substitute for HOME environment
variable. If a prefix ARG is used (‘C-u’) then an absolute path to the
file is used."
  (interactive "P")
  (if buffer-file-name
      (progn
        (let* ((filename buffer-file-name)
               (filename (if (not arg)
                             (string-replace (getenv "HOME") "~" filename)
                           filename))
               (filename (url-encode-url filename))
               (scrim-url (format "scrim://open?file=%s" filename)))

          (message "%s" scrim-url)
          (kill-new scrim-url)))
    (message "Error: No file name associated with buffer: %s" (buffer-name))))

(defun scrim-copy-directory-link (arg)
  "Copy Scrim directory URL into the kill ring with prefix ARG.

Generate a Scrim directory URL “scrim://open” for the current buffer.

By default the path uses the ‘~’ to substitute for HOME environment
variable. If a prefix ARG is used (‘C-u’) then an absolute path to the
file is used."
  (interactive "P")
  (if default-directory
      (progn
        (let* ((dirname default-directory)
               (dirname (if (not arg)
                            (string-replace (getenv "HOME") "~" dirname)
                          dirname))
               (dirname (url-encode-url dirname))
               (scrim-url (format "scrim://open?file=%s" dirname)))
          (message "%s" scrim-url)
          (kill-new scrim-url)))
    (message "Error: Not a Dired buffer.")))

(defun scrim-copy-info-link ()
  "Copy Scrim info URL for current Info node into the kill ring.

Generate a Scrim info node URL “scrim://info” for the current buffer."
  (interactive)
  (if (derived-mode-p 'Info-mode)
      (progn
        (Info-copy-current-node-name)
        (let* ((node-name (car kill-ring))
               (node-name (url-encode-url node-name))
               (scrim-url (format "scrim://info?node=%s" node-name)))

          (message "%s" scrim-url)
          (kill-new scrim-url)))
    (message "Error: Must be in Info buffer.")))

(provide 'scrim-utils)
;;; scrim-utils.el ends here
