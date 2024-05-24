;;; cc-sgml-mode.el --- SGML Mode Customization      -*- lexical-binding: t; -*-

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

(require 'sgml-mode) ;; inherits from text-mode
(require 'display-line-numbers)
(require 'elec-pair)
(require 'hl-line)
(require 'cc-save-hooks)

(add-hook 'sgml-mode-hook #'display-line-numbers-mode)
(add-hook 'sgml-mode-hook #'rainbow-mode)
(add-hook 'sgml-mode-hook #'electric-pair-mode)
(add-hook 'sgml-mode-hook #'sgml-electric-tag-pair-mode)
(add-hook 'sgml-mode-hook #'cc/save-hook-delete-trailing-whitespace)
(add-hook 'sgml-mode-hook #'hl-line-mode)

(keymap-set sgml-mode-map "M-[" #'sgml-skip-tag-backward)
(keymap-set sgml-mode-map "M-]" #'sgml-skip-tag-forward)

(provide 'cc-sgml-mode)
;;; cc-sgml-mode.el ends here
