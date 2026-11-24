;;; cc-python-mode.el --- Python mode configuration  -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2026  Charles Choi

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
(require 'python)
(require 'casual-elisp-utils)

(defun cc/python-mode-setup ()
  "Initialization for `python-mode'."

  (keymap-set python-mode-map "M-[" #'backward-sexp)
  (keymap-set python-mode-map "M-]" #'forward-sexp)

  (keymap-set python-mode-map "C-<up>" #'backward-up-list)
  (keymap-set python-mode-map "C-<down>" #'down-list)
  (keymap-set python-mode-map "C-<left>" #'backward-sexp)
  (keymap-set python-mode-map "C-<right>" #'casual-elisp-next-sexp))

(add-hook 'python-mode-hook #'cc/python-mode-setup)

(provide 'cc-python-mode)
;;; cc-python-mode.el ends here
