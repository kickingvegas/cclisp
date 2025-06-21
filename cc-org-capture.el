;;; cc-org-capture.el --- Org Capture Configuration  -*- lexical-binding: t; -*-

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
(require 'org)
(require 'org-capture)
(require 'org-protocol)

(defun cc/--current-org-default-notes-file ()
  "String path to current daily Org file.
This function is dependent upon this file being created by a daily cron job."
  (cond
   ;; ((string= (system-name) "bingsu.local")
   ;;  (format-time-string "~/org/%Y_%m_%d.org"))
   ((string= (system-name) "dev7")
    "~/Documents/journal/journal.org")
   (t
    (format-time-string "~/org/%Y_%m_%d.org"))))

(defun cc/--find-capture-point-in-file (key)
  "Move point to the end of the first instance of KEY in the current buffer."
  (goto-char (point-min))
  (search-forward key))

(defun cc/--find-capture-point-in-current ()
  "Helper function to locate where to insert capture item in daily Org file."
  (let* ((key (if (string= (system-name) "dev7")
                  "Journal"
                (format-time-string cc/org-daily-header-template))))
    (cc/--find-capture-point-in-file key)))

(defun cc/config-capture-template (&optional prefix suffix)
  "Configure capture template with PREFIX, SUFFIX."
  (let* ((properties (list ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:"))
         (properties (if prefix (append prefix properties) properties))
         (properties (if suffix (append properties suffix) properties)))
    properties))

(defvar cc/org--capture-languages
  (list
   "C"
   "F90"
   "R"
   "awk"
   "clojure"
   "cpp"
   "css"
   "ditaa"
   "dot"
   "elisp"
   "eshell"
   "forth"
   "gnuplot"
   "haskell"
   "java"
   "js"
   "julia"
   "kotlin"
   "latex"
   "lisp"
   "lua"
   "makefile"
   "matlab"
   "max"
   "ocaml"
   "octave"
   "org"
   "perl"
   "plantuml"
   "processing"
   "python"
   "ruby"
   "sass"
   "scheme"
   "sed"
   "shell"
   "sql"
   "sqlite"
   "swift"
   "swiftui"
   )
  "List of supported Org capture languages.")


(defun cc/org--code-select-body ()
  "Body capture code in `kill-ring' head with language prompt."
  (cc/org--capture-code-body-from-kill-ring
   (cc/org--capture-code-choices "elisp")))

(defun cc/org--code-elisp-body ()
  "Body capture Elisp code in `kill-ring' head."
  (cc/org--capture-code-body-from-kill-ring "elisp :lexical no"))

(defun cc/org--code-swift-body ()
  "Body capture Swift code in `kill-ring' head."
  (cc/org--capture-code-body-from-kill-ring "swift"))

(defun cc/org--code-swiftui-body ()
  "Body capture SwiftUI code in `kill-ring' head."
  (cc/org--capture-code-body-from-kill-ring "swiftui"))

(defun cc/org--capture-code-body-from-kill-ring (lang)
  "Generate capture body for LANG code at head of the `kill-ring'."
  (string-join
   (list
    (concat "#+BEGIN_SRC " lang)
    "%c"
    "#+END_SRC")
   "\n"))

(defun cc/org--capture-code-choices (lang)
  "Create selection of programming language choices with default LANG."
  (concat
   "%^{Language|"
   lang
   "|"
   (string-join cc/org--capture-languages "|")
   "}"))


;; Set default Org protocol capture template
(setopt org-protocol-default-template-key "capture")

;; Configure Org capture templates
(setopt org-capture-templates
        '(("a"
           "Appointment"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* %^{description}"
                          "%^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("t"
           "TODO"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* TODO %^{description} %^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("s"
           "Scheduled TODO"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* TODO %^{description} %^G"
                          "SCHEDULED: %^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("c" "Code")


          ("p"
           "Blog Post"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* TODO Post: %^{description} :blog%^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("i"
           "Issue"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* TODO %^{description} %^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "\n** Title"
                          "%?"
                          "** Description\n"
                          "** Environment\n"
                          "** Steps to Reproduce\n"
                          "** Expected Result\n"
                          "** Actual Result\n")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("j"
           "Journal"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("%(datestamp2)"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)


          ("r"
           "BeOrg Reminder"
           entry
           (file "~/org/beorg.org")
           (function (lambda ()
                       (string-join
                        '("* TODO %^{description}"
                          "SCHEDULED: %^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :empty-lines 1)

          ("n"
           "Note"
           entry
           (file "")              ; this will persist in org-default-notes-file
           (function (lambda ()
                       (string-join
                        '("* %^{description}"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("S"
           "Song"
           entry
           (file+headline
            "~/org/songs/songs.org"
            "Songs")
           (function (lambda ()
                       (string-join
                        '("* %^{Song}"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":ARTIST: %^{Artist}"
                          ":END:"
                           "%?")
                        "\n")))
           :empty-lines 1
           :prepend t)

          ;; ("o" "Org Protocol Templates")

          ("wwdc"
           "WWDC Session (Org Protocol)"
           entry
           (file+headline
            "~/org/wwdc25.org"
            "WWDC 25 Notes")
           (function (lambda ()
                       (string-join
                        '("* TODO %:description"
                          "%:annotation"
                          "%i"
                          "%?")
                        "\n")))
           :immediate-finish 1
           :empty-lines 1)

          ("journal"
           "Journal (Org Protocol)"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("%(datestamp2)"
                          "%i")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("capture"
           "Capture (Org Protocol)"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        '("* %:description"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%:annotation"
                          "%i"
                          ""
                          "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)

          ("code"
           "Source Code (Org Protocol)"
           entry
           (file+function
            cc/--current-org-default-notes-file
            cc/--find-capture-point-in-current)
           (function (lambda ()
                       (string-join
                        (list "* Source: %:description"
                              ":PROPERTIES:"
                              ":CREATED: %U"
                              ":END:"
                              "%:link"
                              (concat "#+BEGIN_SRC "
                                      (cc/org--capture-code-choices "elisp"))
                              "%i"
                              "#+END_SRC"
                              ""
                              "%?")
                        "\n")))
           :prepend t
           :empty-lines 1)
          ))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-to-list 'org-capture-templates
                '("cc"
                  "Code"
                  plain
                  (here)
                  (function cc/org--code-select-body)
                  :empty-lines 1
                  :immediate-finish 1)
                t)

   (add-to-list 'org-capture-templates
                '("ce"
                  "Elisp"
                  plain
                  (here)
                  (function cc/org--code-elisp-body)
                  :empty-lines 1
                  :immediate-finish 1)
                t)

   (add-to-list 'org-capture-templates
                '("cs"
                  "Swift"
                  plain
                  (here)
                  (function cc/org--code-swift-body)
                  :empty-lines 1
                  :immediate-finish 1)
                t)

   (add-to-list 'org-capture-templates
                '("cu"
                  "SwiftUI"
                  plain
                  (here)
                  (function cc/org--code-swiftui-body)
                  :empty-lines 1
                  :immediate-finish 1)
                t)))

(defun cc/org-capture-template-keys ()
  "List out capture template keys."
  (interactive)
  (let* ((templates (mapcar
                     (lambda (x) (list (nth 0 x) (nth 1 x)))
                     org-capture-templates))
         (buflist (mapcar
                   (lambda (x)
                     (format "%17s %s" (nth 0 x) (nth 1 x)))
                   templates)))
    (message (string-join buflist "\n"))))

(provide 'cc-org-capture)
;;; cc-org-capture.el ends here
