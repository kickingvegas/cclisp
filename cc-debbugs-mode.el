;;; cc-debbugs-mode.el --- Debbugs Customization     -*- lexical-binding: t; -*-

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
(require 'debbugs-gnu)
(require 'hl-line)
(require 'gnus-sum)
(require 'casual-lib)


(add-hook 'debbugs-gnu-mode-hook #'hl-line-mode)

(keymap-set gnus-summary-mode-map "p" #'gnus-summary-prev-article)
(keymap-set gnus-summary-mode-map "n" #'gnus-summary-next-article)

(keymap-set gnus-summary-mode-map "P" #'gnus-summary-prev-unread-article)
(keymap-set gnus-summary-mode-map "N" #'gnus-summary-next-unread-article)


(transient-define-prefix cc/debbugs-tmenu ()
  ["Debbugs "
   ["Emacs"
    ("b" "Bugs" debbugs-gnu-bugs)
    ("p" "Package" debbugs-gnu-package)
    ("m" "My Bugs" debbugs-gnu-my-open-bugs)]

   ["Org"
    ("o" "Org" debbugs-org-bugs)
    ]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("q" "Quit" quit-window)])

(keymap-set debbugs-gnu-mode-map "C-o" #'cc/debbugs-tmenu)

(provide 'cc-debbugs-mode)
;;; cc-debbugs-mode.el ends here
