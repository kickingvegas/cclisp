;;; cc-ibuffer-mode.el --- ibuffer configuration     -*- lexical-binding: t; -*-

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

(require 'ibuffer)
(require 'ibuf-ext)
(require 'hl-line)
(require 'mouse)
(require 'casual-ibuffer)
(require 'avy)

(add-hook 'ibuffer-mode-hook #'hl-line-mode)
(add-hook 'ibuffer-mode-hook #'ibuffer-auto-mode)

(keymap-set ibuffer-name-map "<mouse-1>" #'mouse-set-point)
(keymap-set ibuffer-name-map "<double-mouse-1>" #'ibuffer-mouse-visit-buffer)
(keymap-set ibuffer-name-map "<mouse-2>" #'ibuffer-mouse-toggle-mark)

(keymap-set ibuffer-mode-filter-group-map
            "<mouse-1>" #'mouse-set-point)
(keymap-set ibuffer-mode-filter-group-map
            "<double-mouse-1>" #'ibuffer-mouse-toggle-filter-group)
(keymap-set ibuffer-mode-filter-group-map
            "<mouse-2>" #'ibuffer-mouse-toggle-mark)

(keymap-set ibuffer-mode-map "<f1>" #'avy-goto-line)
(keymap-set ibuffer-mode-map "M-<f1>" #'ibuffer-jump-to-buffer)
(keymap-set ibuffer-mode-map "s-<f1>" #'ibuffer-jump-to-filter-group)
(keymap-set ibuffer-mode-map "<f2>" #'avy-goto-line)

(keymap-set ibuffer-mode-map "C-o" #'casual-ibuffer-tmenu)
(keymap-set ibuffer-mode-map "F" #'casual-ibuffer-filter-tmenu)
(keymap-set ibuffer-mode-map "s" #'casual-ibuffer-sortby-tmenu)

(keymap-set ibuffer-mode-map "{" #'ibuffer-backwards-next-marked)
(keymap-set ibuffer-mode-map "}" #'ibuffer-forward-next-marked)
(keymap-set ibuffer-mode-map "[" #'ibuffer-backward-filter-group)
(keymap-set ibuffer-mode-map "]" #'ibuffer-forward-filter-group)
(keymap-set ibuffer-mode-map "$" #'ibuffer-toggle-filter-group)
(keymap-set ibuffer-mode-map "J" #'ibuffer-jump-to-filter-group)

(provide 'cc-ibuffer-mode)
;;; cc-ibuffer-mode.el ends here
