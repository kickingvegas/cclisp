;;; cc-repeat-mode.el --- Repeat Mode Customization -*- lexical-binding: t; -*-

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

;;; Code:
(require 'org)

(defvar vifon/buffer-nextprev-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    #'next-buffer)
    (define-key map (kbd "<right>") #'next-buffer)
    (define-key map (kbd "<down>")  #'previous-buffer)
    (define-key map (kbd "<left>")  #'previous-buffer)
    map))
;;(put 'next-buffer     'repeat-map 'vifon/buffer-nextprev-repeat-map)
;;(put 'previous-buffer 'repeat-map 'vifon/buffer-nextprev-repeat-map)

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(defvar cc/org-header-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p")    #'org-previous-visible-heading)
    (define-key map (kbd "n")  #'org-next-visible-heading)
    map))

(repeatize 'vifon/buffer-nextprev-repeat-map)
(repeatize 'cc/org-header-navigation-repeat-map)

(repeat-mode)

(provide 'cc-repeat-mode)
;;; cc-repeat-mode.el ends here
