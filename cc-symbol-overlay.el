;;; cc-symbol-overlay.el --- Configuration for Symbol Overlay  -*- lexical-binding: t; -*-

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
;;(require 'symbol-overlay)
;;(require 'casual-lib)
;; (require 'casual-symbol-overlay)
;; (keymap-set symbol-overlay-map "C-o" #'casual-symbol-overlay-tmenu)

(use-package casual-symbol-overlay
  :ensure nil
  :bind (:map
         symbol-overlay-map
         ("C-o" . casual-symbol-overlay-tmenu)))


(provide 'cc-symbol-overlay)
;;; cc-symbol-overlay.el ends here
