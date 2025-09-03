;;; cc-erc-mode.el --- ERC customization             -*- lexical-binding: t; -*-

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
(require 'erc-nicks)
(require 'casual-lib)

(transient-define-prefix casual-erc-tmenu ()
  "Transient menu for erc."

  ["Casual: ERC"
   ["Channel"
    ("s" "Switch…" erc-switch-to-buffer :transient t)
    ("x" "Clear" erc-kill-input)
    ("a" "BoL" erc-bol)]

   ["Navigation"
    ("<prior>" "Page Up" scroll-down-command :transient t)
    ("<next>" "Page Down" scroll-up-command :transient t)]

   ["Misc"
    ("n" "Names" erc-channel-names :transient t)
    ("f" "Toggle Flood" erc-toggle-flood-control :transient t)
    ("b" "Toggle Bufbar" erc-bufbar-mode :transient t)]
   ]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Dismiss" transient-quit-all)
          (casual-lib-quit-all)
          ("Q" "Quit" erc-quit-server)])

(defun cc/configure-erc-tty ()
  "Set ERC nickname background to black on TTY."
  (if (not (display-graphic-p))
      (setopt erc-nicks-bg-color "black")))

(keymap-set erc-mode-map "M-m" #'casual-erc-tmenu)
(keymap-set erc-mode-map "C-c m" #'casual-erc-tmenu)
(keymap-set erc-mode-map "<f1>" #'erc-switch-to-buffer)

(cc/configure-erc-tty)

(provide 'cc-erc-mode)
;;; cc-erc-mode.el ends here
