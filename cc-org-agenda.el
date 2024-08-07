;;; cc-org-agenda.el --- Org Agenda Configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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
(require 'casual-agenda)
(require 'bookmark)

(keymap-set org-agenda-mode-map "C-o" #'casual-agenda-tmenu)
(keymap-set org-agenda-mode-map "M-j" #'org-agenda-clock-goto)
(keymap-set org-agenda-mode-map "J" #'bookmark-jump)

;; (use-package casual-agenda
;;   :ensure nil
;;   :bind (:map
;;          org-agenda-mode-map
;;          ("C-o" . casual-agenda-tmenu)
;;          ("M-j" . org-agenda-clock-goto) ; optional
;;          ("J" . bookmark-jump))) ; optional

(provide 'cc-org-agenda)
;;; cc-org-agenda.el ends here
