;;; cc-tetris-mode.el --- Tetris Customization -*- lexical-binding: t; -*-

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

(require 'tetris)

;;; Code:

(keymap-set tetris-mode-map "j" 'tetris-move-left)
(keymap-set tetris-mode-map "i" 'tetris-rotate-prev)
(keymap-set tetris-mode-map "k" 'tetris-rotate-next)
(keymap-set tetris-mode-map "l" 'tetris-move-right)
(keymap-set tetris-mode-map "m" 'tetris-move-down)

(provide 'cc-tetris-mode)
;;; cc-tetris-mode.el ends here
