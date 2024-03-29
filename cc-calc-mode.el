;;; cc-calc-mode.el --- Calc customization           -*- lexical-binding: t; -*-

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

(require 'calc)
(require 'casual)

(defun cc/confirm-before-calc-quit ()
  "Raise confirm prompt before invoking `calc-quit'."
  (interactive)
  (if (y-or-n-p "Really Quit? ")
      (calc-quit)
    (message "all good")))

;;(define-key calc-mode-map (kbd "q") 'cc/confirm-before-calc-quit)
(define-key calc-mode-map (kbd "C-o") 'casual-main-menu)

(when (eq window-system 'mac)
  (setq calc-gnuplot-default-device "aqua"))

(provide 'cc-calc-mode)
;;; cc-calc-mode.el ends here
