;;; cc-bookmarks-bmenu-mode.el --- Bookmarks Bmenu Mode Config  -*- lexical-binding: t; -*-

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
(require 'casual-bookmarks)

(easy-menu-add-item global-map '(menu-bar)
                    casual-bookmarks-main-menu
                    "Tools")

(keymap-set bookmark-bmenu-mode-map "C-o" #'casual-bookmarks-tmenu)
(keymap-set bookmark-bmenu-mode-map "S" #'casual-bookmarks-sortby-tmenu)
(keymap-set bookmark-bmenu-mode-map "J" #'bookmark-jump)

(provide 'cc-bookmarks-bmenu-mode)
;;; cc-bookmarks-bmenu-mode.el ends here
