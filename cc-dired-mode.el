;;; cc-dired-mode.el --- Dired Customization -*- lexical-binding: t -*-

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
(require 'dired)
(require 'dired-x)
(require 'cclisp)
(require 'wdired)
(require 'image-dired)
(require 'casual-dired)
(require 'cc-main-tmenu)

(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'dired-async-mode)
(add-hook
 'dired-mode-hook
 (lambda ()
   (setq-local mouse-1-click-follows-link 'double)))

(define-key dired-mode-map (kbd "M-o") #'dired-omit-mode)
(define-key dired-mode-map (kbd "C-M-o") #'cc/main-tmenu)
(define-key dired-mode-map (kbd "E") #'wdired-change-to-wdired-mode)
(define-key dired-mode-map (kbd "C-o") #'casual-dired-tmenu)
(define-key dired-mode-map (kbd "s") #'casual-dired-sort-by-tmenu)
(define-key dired-mode-map (kbd "M-n") #'dired-next-dirline)
(define-key dired-mode-map (kbd "M-p") #'dired-prev-dirline)
(define-key dired-mode-map (kbd "]") #'dired-next-subdir)
(define-key dired-mode-map (kbd "[") #'dired-prev-subdir)
(define-key dired-mode-map (kbd "A-M-<mouse-1>") #'browse-url-of-dired-file)

(define-key image-dired-thumbnail-mode-map (kbd "n") #'image-dired-display-next)
(define-key image-dired-thumbnail-mode-map (kbd "p") #'image-dired-display-previous)

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
