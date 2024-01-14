;;; cc-wgrep-mode.el --- wgrep configuration -*- lexical-binding: t; -*-

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

(require 'wgrep)

(easy-menu-define cc/wgrep-menu nil
  "Keymap for wgrep menu"
  '("Writeable Grep"
    :visible (derived-mode-p 'grep-mode)
    ["Change to wgrep mode" wgrep-change-to-wgrep-mode
     :enable (equal buffer-read-only t)
     :help "Change to wgrep mode."]

    ["Finish Edit" wgrep-finish-edit
     :enable (not buffer-read-only)
     :help "Apply the changes to file buffers and exit."]

    ["Mark Current Line for Deletion" wgrep-mark-deletion
     :enable (not buffer-read-only)
     :help "Mark as delete to current line (including newline)."]

    ["Toggle Readonly" wgrep-toggle-readonly-area
     :enable (not buffer-read-only)
     :help "Toggle read-only area to remove a whole line."]

    ["Remove Change" wgrep-remove-change
     :enable (not buffer-read-only)
     :help "Remove changes in the region between BEG and END."]

    ["Remove All Changes" wgrep-remove-all-change
     :enable (not buffer-read-only)
     :help "Remove changes in the whole buffer."]

    ["Abort Changes and Exit" wgrep-abort-changes
     :enable (not buffer-read-only)
     :help "Discard all changes and return to original mode."]

    ["Exit" wgrep-exit
     :enable (not buffer-read-only)
     :help "Return to original mode."]))

(easy-menu-add-item grep-menu-map nil
                    cc/wgrep-menu
                    nil)

(provide 'cc-wgrep-mode)
;;; cc-wgrep-mode.el ends here
