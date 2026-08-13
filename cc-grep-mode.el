;;; cc-grep-mode.el --- grep mode customization      -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2026  Charles Choi

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

(require 'compile)
(require 'grep)
(require 'hl-line)
(require 'wgrep)

(easy-menu-define cc/wgrep-menu nil
  "Keymap for wgrep menu."
  '("Writeable Grep"
    :visible (eq (current-local-map) wgrep-mode-map)
    :enable (not buffer-read-only)

    ["Finish Edit" wgrep-finish-edit
     :help "Apply the changes to file buffers and exit."]

    ["Mark Current Line for Deletion" wgrep-mark-deletion
     :help "Mark as delete to current line (including newline)."]

    ["Toggle Readonly" wgrep-toggle-readonly-area
     :help "Toggle read-only area to remove a whole line."]

    ["Remove Change" wgrep-remove-change
     :help "Remove changes in the region between BEG and END."]

    ["Remove All Changes" wgrep-remove-all-change
     :help "Remove changes in the whole buffer."]

    ["Abort Changes and Exit" wgrep-abort-changes
     :help "Discard all changes and return to original mode."]

    ["Exit" wgrep-exit
     :help "Return to original mode."]))

(easy-menu-add-item grep-menu-map nil
                    ["Writable Grep" wgrep-change-to-wgrep-mode
                     :enable buffer-read-only
                     :help "Change to wgrep mode."]
                    nil)

(easy-menu-add-item global-map '(menu-bar) cc/wgrep-menu "Tools")




(provide 'cc-grep-mode)
;;; cc-grep-mode.el ends here
