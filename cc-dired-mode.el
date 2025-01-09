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
(require 'image-dired-dired)
(require 'casual-dired)
(require 'casual-editkit)
(require 'cc-main-tmenu)

;; (use-package image-dired
;;   :defer t
;;   :bind (:map
;;          image-dired-thumbnail-mode-map
;;          ("n" . #'image-dired-display-next)
;;          ("p" . #'image-dired-display-previous))
;;   :after (dired))

;; (use-package image-dired-dired
;;   :defer t
;;   :bind (:map
;;          dired-mode-map
;;          (";" . #'image-dired-dired-toggle-marked-thumbs))
;;   :after (image-dired))

;; (use-package wdired
;;   :defer t
;;   :bind (:map
;;          dired-mode-map
;;          ("E" . #'wdired-change-to-wdired-mode))
;;   :after (dired))

;; (use-package dired-x
;;   :defer t
;;   :after (dired))


;; (use-package browse-url
;;   :defer t)

;; (use-package cclisp :defer t)
;; (use-package cc-main-tmenu :defer t)

;; (use-package dired
;;   :bind (:map
;;          dired-mode-map
;;          ("M-o" . #'dired-omit-mode)
;;          ("M-p" . #'dired-prev-dirline)
;;          ("M-n" . #'dired-next-dirline)
;;          ("[" . #'dired-prev-subdir)
;;          ("]" . #'dired-next-subdir)
;;          ("M-j" . #'dired-goto-subdir)
;;          ("TAB" . #'dired-next-subdir)
;;          ("<backtab>" . #'dired-prev-subdir)
;;          ("A-M-<mouse-1>" . #'browse-url-of-dired-file)
;;          ("C-M-o" . #'casual-editkit-main-tmenu))
;;   :after (browse-url cclisp cc-main-tmenu))

;; (use-package casual-dired
;;   :bind (:map dired-mode-map
;;               ("C-o" . #'casual-dired-tmenu)
;;               ("s" . #'casual-dired-sort-by-tmenu)
;;               ("/" . #'casual-dired-search-replace-tmenu))
;;   )
;;   ;; :after (dired image-dired image-dired-dired wdired))

;; (use-package mouse
;;   :hook ((dired-mode . context-menu-mode)))

;; (use-package dired-async
;;   :hook ((dired-mode . dired-async-mode)))

;; (use-package hl-line
;;   :hook ((dired-mode . hl-line-mode)))


(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)
(add-hook 'dired-mode-hook 'dired-async-mode)
(add-hook
 'dired-mode-hook
 (lambda ()
   (setq-local mouse-1-click-follows-link 'double)))

(keymap-set dired-mode-map "<mouse-2>" #'dired-mouse-find-file)
(keymap-set dired-mode-map "M-o" #'dired-omit-mode)
(keymap-set dired-mode-map "C-M-o" #'casual-editkit-main-tmenu)
(keymap-set dired-mode-map "E" #'wdired-change-to-wdired-mode)
(keymap-set dired-mode-map "C-o" #'casual-dired-tmenu)
(keymap-set dired-mode-map "s" #'casual-dired-sort-by-tmenu)
(keymap-set dired-mode-map "/" #'casual-dired-search-replace-tmenu)
(keymap-set dired-mode-map "M-n" #'dired-next-dirline)
(keymap-set dired-mode-map "M-p" #'dired-prev-dirline)
(keymap-set dired-mode-map "]" #'dired-next-subdir)
(keymap-set dired-mode-map "[" #'dired-prev-subdir)
(keymap-set dired-mode-map "M-j" #'dired-goto-subdir)
(keymap-set dired-mode-map ";" #'image-dired-dired-toggle-marked-thumbs)

;; Added to be consistent with IBuffer
(keymap-set dired-mode-map "<backtab>" #'dired-prev-subdir)
(keymap-set dired-mode-map "TAB" #'dired-next-subdir)

(keymap-set dired-mode-map "A-M-<mouse-1>" #'browse-url-of-dired-file)

(keymap-set image-dired-thumbnail-mode-map "n" #'image-dired-display-next)
(keymap-set image-dired-thumbnail-mode-map "p" #'image-dired-display-previous)

(add-hook 'wdired-mode-hook 'superword-mode)

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
