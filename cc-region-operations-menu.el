;;; cc-region-operations-menu.el --- Region Operations Menu -*- lexical-binding: t; -*-

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

(require 'cc-context-menu-macros)

;;; Code:

(easy-menu-define cc/region-operations-menu nil
  "Keymap for Region Operations submenu."
  '("Operate on Region"
    :visible (region-active-p)
    ["Search with Google" google-this-noconfirm
     :enable (region-active-p)
     :label (cc/context-menu-label "Search with Google")
     :help "Search Google with selected region"]

    ["Translate" google-translate-smooth-translate
     :enable (region-active-p)
     :label (concat (cc/context-menu-label "Translate") "…")
     :help "Translate selected region with Google Translate"]

    ["Upload to Webpaste" webpaste-paste-region
     :enable (region-active-p)
     :label (cc/context-menu-label "Upload to Webpaste")
     :help "Upload selected region to paste service leaving \
link in the clipboard"]

    ["Start Speaking" cc/say-region
     :enable (region-active-p)
     :help "Start speaking selected region"]

    ["Call" cc/call-nanp-phone-number
     :enable (region-active-p)
     :label (cc/context-menu-label "Call")
     :visible (cc/nanp-phone-number-p)
     :help "Call phone number"]

    ["Open in Apple Maps" cc/open-region-in-apple-maps
     :enable (region-active-p)
     :label (cc/context-menu-label "Open in Apple Maps")
     :visible (not (cc/nanp-phone-number-p))
     :help "Open in Apple Maps"]))

(provide 'cc-region-operations-menu)
;;; cc-region-operations-menu.el ends here
