;;; mdired.el --- Commands for marked Dired items.   -*- lexical-binding: t; -*-

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

;; Commands for marked Dired items.

;; https://superuser.com/questions/176627/in-emacs-dired-how-can-i-run-a-command-on-multiple-marked-files

;;; Code:
(require 'dired)
(require 'compile)

(defun mdired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun mdired-copy (&optional arg)
  "Copy current Dired item to marked Dired items for ARG."
  (interactive "P" dired-mode)
  (let* ((arg (if (not arg)
                  (completing-read "Overwrite Policy: "
                                   '("never" "overwrite" "ask")
                                   nil nil "ask")
                "overwrite"))
         (ok-flag (cond
                   ((string-equal "never" arg) nil)
                   ((string-equal "overwrite" arg) t)
                   ((string-equal "ask" arg) 1)))
         (source (dired-get-filename))
         (targets (dired-get-marked-files)))

    (mapc (lambda (target)
            (let ((target (if (and (file-directory-p target)
                                   (not (string-equal (substring target -1) "/")))
                              (concat target "/")
                            target)))
              (copy-file source target ok-flag)))
          targets)))

(defun mdired-directory-p (targets)
  "Predicate if all elements in TARGETS are directories."
  (let ((tests (mapcar (lambda (target)
                          (file-directory-p target))
                        targets)))
    (seq-reduce (lambda (a b)
                  (and a b))
                tests t)))

(defun mdired-compile ()
  "Run `compile' on each marked directory."
  (interactive nil dired-mode)

  (let* ((command (compilation-read-command (eval compile-command)))
         (targets (dired-get-marked-files)))

    (if (mdired-directory-p targets)
        (mapc (lambda (target)
                (let ((new-name (format "*compilation - %s*" (file-name-nondirectory
                                                              (directory-file-name target))))
                      (default-directory target))
                  (compilation-start command nil (lambda (_mode)
                                                   new-name))))
              targets)
      (error "ERROR: All marked files must be directories"))))


(defun mdired-byte-compile ()
  "Run `compile' on each marked directory."
  (interactive nil dired-mode)

  (let ((targets (dired-get-marked-files)))
    (mapc (lambda (target)
            (cond
             ((file-directory-p target)
              (let* ((default-directory target))
                (byte-recompile-directory target)))

             ((string-equal (file-name-extension target) "el")
              (byte-recompile-file target))

             (t
              (error "Invalid target for byte-compilation: %s" target))))
              targets)))

(provide 'mdired)
;;; mdired.el ends here
