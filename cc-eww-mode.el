;;; cc-eww-mode.el --- EWW Mode                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026  Charles Choi

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
(require 'eww)
(require 'hl-line)
(require 'bookmark)
(require 'avy)
(require 'casual-eww)

(add-hook 'eww-mode-hook #'hl-line-mode)
(add-hook 'eww-bookmark-mode-hook #'hl-line-mode)

(defun cc/eww-point-on-first-line-p ()
  "Return t if the point is on the first line, nil otherwise.

This function taken via GitHub Copilot query."
  (= (line-number-at-pos) 1))

(defun cc/eww-point-on-last-line-p ()
  "Return t if the point is on the last line, nil otherwise.

This function taken via GitHub Copilot query."
  (let ((current-line (line-number-at-pos))
        (total-lines (length eww-bookmarks))
        ;; (total-lines (count-lines (point-min) (point-max)))
        )
    (= current-line total-lines)))

(defun cc/eww-bookmark-reorder-down ()
  "Reorder bookmark down list."
  (interactive)
  (if (not (cc/eww-point-on-last-line-p))
      (progn
        (eww-bookmark-kill)
        (forward-line)
        (eww-bookmark-yank)
        (forward-line -1))))

(defun cc/eww-bookmark-reorder-up ()
  "Reorder bookmark up list."
  (interactive)
  (if (not (cc/eww-point-on-first-line-p))
      (progn
        (eww-bookmark-kill)
        (forward-line -1)
        (eww-bookmark-yank)
        (forward-line -1))))

(keymap-set eww-bookmark-mode-map "C-o" #'casual-eww-bookmarks-tmenu)
(keymap-set eww-bookmark-mode-map "p" #'previous-line)
(keymap-set eww-bookmark-mode-map "n" #'next-line)
(keymap-set eww-bookmark-mode-map "M-p" #'cc/eww-bookmark-reorder-up)
(keymap-set eww-bookmark-mode-map "M-n" #'cc/eww-bookmark-reorder-down)

(keymap-set eww-bookmark-mode-map "<double-mouse-1>" #'eww-bookmark-browse)

(keymap-set eww-mode-map "C-o" #'casual-eww-tmenu)
(keymap-set eww-mode-map "C-c C-o" #'eww-browse-with-external-browser)

(keymap-set eww-mode-map "j" #'shr-next-link)
(keymap-set eww-mode-map "k" #'shr-previous-link)

(keymap-set eww-mode-map "[" #'eww-previous-url)
(keymap-set eww-mode-map "]" #'eww-next-url)

(keymap-set eww-mode-map "M-]" #'eww-forward-url)
(keymap-set eww-mode-map "M-[" #'eww-back-url)

(keymap-set eww-mode-map "<f1>" #'avy-goto-char-timer)

(keymap-set eww-mode-map "n" #'casual-lib-browse-forward-paragraph)
(keymap-set eww-mode-map "p" #'casual-lib-browse-backward-paragraph)

(keymap-set eww-mode-map "P" #'casual-eww-backward-paragraph-link)
(keymap-set eww-mode-map "N" #'casual-eww-forward-paragraph-link)

;;(keymap-set eww-mode-map "p" #'backward-paragraph)

;; (keymap-set eww-mode-map "n" #'next-line)
;; (keymap-set eww-mode-map "p" #'previous-line)

(keymap-set eww-mode-map "M-l" #'eww)

(provide 'cc-eww-mode)
;;; cc-eww-mode.el ends here
