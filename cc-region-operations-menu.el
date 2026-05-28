;;; cc-region-operations-menu.el --- Region Operations Menu -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026 Charles Choi

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

(require 'google-this)
(require 'google-translate)
(require 'webpaste)
(require 'cclisp)
(require 'anju-utils)

;; (require 'cc-context-menu-macros)

;;; Code:

(easy-menu-define cc/region-operations-menu nil
  "Keymap for Region Operations submenu."
  '("Operate on Region"
    :enable (use-region-p)

    ["Search with Google" google-this-noconfirm
     :label (anju-menu-label "Search with Google")
     :help "Search Google with selected region"]

    ["Translate" google-translate-smooth-translate
     :label (anju-menu-label "Translate")
     :help "Translate selected region with Google Translate"]

    ["Upload to Webpaste" webpaste-paste-region
     :label (anju-menu-label "Upload to Webpaste")
     :help "Upload selected region to paste service leaving \
link in the clipboard"]

    ["Start Speaking" cc/say-region
     :visible (eq window-system 'ns)
     :help "Start speaking selected region"]

    ["Call" cc/call-nanp-phone-number
     :label (anju-menu-label "Call")
     :visible (and (cc/nanp-phone-number-p) (eq window-system 'ns))
     :help "Call phone number"]

    ["Open in Apple Maps" cc/open-region-in-apple-maps
     :label (anju-menu-label "Open in Apple Maps")
     :visible (and (not (cc/nanp-phone-number-p)) (eq window-system 'ns))
     :help "Open in Apple Maps"]))

(provide 'cc-region-operations-menu)
;;; cc-region-operations-menu.el ends here
