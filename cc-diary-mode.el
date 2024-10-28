;;; cc-diary-mode.el --- diary customization         -*- lexical-binding: t; -*-

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

(require 'diary-lib)

(add-hook 'diary-list-entries-hook #'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)

(add-hook 'diary-nongregorian-listing-hook 'diary-chinese-list-entries)
(add-hook 'diary-nongregorian-marking-hook 'diary-chinese-mark-entries)

(provide 'cc-diary-mode)
;;; cc-diary-mode.el ends here
