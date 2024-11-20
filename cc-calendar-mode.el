;;; cc-calendar-mode.el --- Calendar Configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>
;; Keywords: calendar

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
(require 'calendar)
(require 'casual-lib)
(require 'casual-calendar)

;; '(org-agenda-include-diary t)
;; (add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
;; (add-hook 'diary-list-entries-hook #'diary-sort-entries t)

;; (add-hook 'diary-nongregorian-listing-hook 'diary-chinese-list-entries)
;; (add-hook 'diary-nongregorian-marking-hook 'diary-chinese-mark-entries)
;; (add-hook 'calendar-mode-hook #'diary-mark-entries)

;; '(calendar-latitude 37.7643)
;; '(calendar-location-name '(format "%s, San Francisco" "Inner Sunset"))
;; '(calendar-longitude -122.4753)
;; '(calendar-mark-holidays-flag t)
;; '(calendar-move-hook '(calendar-update-mode-line))



(keymap-set calendar-mode-map "C-o" #'casual-calendar)
; (add-hook 'calendar-mode-hook #'calendar-mark-holidays)
(add-hook 'calendar-mode-hook #'diary-mark-entries)

(provide 'cc-calendar-mode)
;;; cc-calendar-mode.el ends here
