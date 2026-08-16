;;; cc-pwa.el --- PWA routines                       -*- lexical-binding: t; -*-

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

;;; PWA
(require 'map)

(defcustom cc-pwa-apps
  '(("connect" . "~/Applications/Connect.app")
    ("github" . "~/Applications/GitHub.app")
    ("google-translate" . "~/Applications/Google Translate.app")
    ("instagram" . "~/Applications/Instagram.app")
    ("kanopy" . "~/Applications/Kanopy.app")
    ("netflix" . "~/Applications/Netflix.app")
    ("peacock" . "~/Applications/Peacock.app")
    ("reddit" . "~/Applications/Reddit.app")
    ("sfba" . "~/Applications/SFBA.social.app")
    ("twitch" . "~/Applications/Twitch.app")
    ("youtube" . "~/Applications/YouTube.app"))
  "List of PWAs."
  :type '(alist :key-type (string :tag "Name")
                :value-type (file :tag "Bundle Path"))
  :group 'kickingvegas)

(defvar cc/pwa-table nil
  "Alist for Safari PWA entries.")

(defun cc/open-safari-pwa (bundle-path &optional url)
  "Open Safari PWA with BUNDLE-PATH and URL."
  (let* ((safari-id
          (let ((test-id (map-elt cc/pwa-table bundle-path)))
            (if test-id
                test-id
              (let ((extract-id (cc/pwa-extract-bundleid bundle-path)))
                (setq cc/pwa-table
                      (map-insert cc/pwa-table bundle-path extract-id))
                extract-id)))))
    (if url
        (process-lines "open" "-b" safari-id url)
      (process-lines "open" "-b" safari-id))))

(defun pwa (&optional key url)
  "Launch PWA with KEY and URL."
  (interactive)
  (let* ((key (if (not key)
                  (completing-read "App: " (map-keys cc-pwa-apps))
                key))
         (bundle-path (if (map-contains-key cc-pwa-apps key)
                          (map-elt cc-pwa-apps key)
                        nil)))
    (if bundle-path
        (cc/open-safari-pwa bundle-path url)
      (error "Invalid app name: %s" key))))


(defun cc/pwa-extract-bundleid (pwa-path)
  "Extract Bundle ID from PWA-PATH."
  (let* ((plist-path (file-name-concat (expand-file-name pwa-path) "Contents" "Info.plist"))
         (cmd-list (list "plutil"
                         "-extract"
                         "CFBundleIdentifier"
                         "raw"
                         "-o"
                         "-"
                         (format "\"%s\"" plist-path)))
         (cmd (string-join cmd-list " "))
         (result (string-trim (shell-command-to-string cmd))))
    result))

(defun cc/pwa-load-table ()
  "Initialize `cc/pwa-table'."
  (interactive)
  (let ((pwa-paths (map-values cc-pwa-apps)))
    (mapc (lambda (x)
            (unless (map-elt cc/pwa-table x)
              (setq cc/pwa-table
                  (map-insert cc/pwa-table x (cc/pwa-extract-bundleid x)))))
          pwa-paths)))

(provide 'cc-pwa)
;;; cc-pwa.el ends here
