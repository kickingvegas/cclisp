;;; cc-google-translate.el --- Google Translate Customization -*- lexical-binding: t; -*-

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

(require 'google-translate)
(require 'google-translate-smooth-ui)
;;(global-set-key "\C-ct" 'google-translate-smooth-translate)

;;; Code:

(setq google-translate-translation-directions-alist
      '(("ko" . "en") ("en" . "ko")))

(provide 'cc-google-translate)
;;; cc-google-translate.el ends here
