;;; recent-rgrep.el --- Sort rgrep by modification time  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; URL: https://github.com/kickingvegas/recent-rgrep
;; Keywords: tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

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

(defcustom recent-rgrep-glob-extensions
  '("*"
    "*.asm"
    "*.c"
    "*.c++"
    "*.cpp"
    "*.css"
    "*.csv"
    "*.dot"
    "*.el"
    "*.gnuplot"
    "*.go"
    "*.gv"
    "*.h"
    "*.htm"
    "*.html"
    "*.js"
    "*.json"
    "*.log"
    "*.m"
    "*.markdown"
    "*.md"
    "*.org"
    "*.pl"
    "*.plantuml"
    "*.py"
    "*.rb"
    "*.rs"
    "*.sh"
    "*.sql"
    "*.swift"
    "*.txt"
    "*.xml")
  "List of glob-conforming text extensions."
  :group 'grep
  :type '(repeat string))


(defun recent-rgrep (query)
  "Search QUERY in files recursively and sort by recent modification time.

From the current directory, this command shall recursively search
files for lines that match the pattern QUERY that is entered with
the first prompt. The results are sorted in recently modified
file order with the most recent files presented at the top of the
buffer. By default, matching QUERY is case-insensitive.

A second prompt to filter the extent of files to search is
presented. By default, the extent of files searched will be all
non-binary files. Pressing enter with no value on the second
prompt will accomplish this. If filtering is desired, then the
user can specify a file name glob expression via completion.

The list of different file name glob expressions can be
controlled via the customizable variable
`recent-rgrep-glob-extensions'.

The format of the QUERY and the glob expression must be GNU grep
compatible.

If this command is invoked with a prefix (C-u) then the search
will be case-sensitive.

* Implementation Details

This command invokes the Bash script recent-rgrep which in turn
invokes grep to do a recursive search of QUERY.

This script is configured to only look at non-binary files. It
will also not look into SCM directories such as .git.

* References

- Info node `(grep) Top'
- Info node `(grep) File and Directory Selection'"

  (interactive "sSearch regexp: ")

  (let* ((file-pattern (completing-read
                        "In files with extension (default: all): "
                        recent-rgrep-glob-extensions))
         (dir default-directory)
         (commands (list)))

    (push "recent-rgrep" commands)
    (if current-prefix-arg
        (push "-c" commands))

    (if (not (string= file-pattern ""))
        (push (format "-f '%s'" file-pattern) commands))

    (push (format "'%s'" query) commands)

    (compilation-start (string-join (reverse commands) " ") #'grep-mode)
    (if (eq next-error-last-buffer (current-buffer))
	(setq default-directory dir))))

(provide 'recent-rgrep)
;;; recent-rgrep.el ends here
