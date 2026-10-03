;;; cc-music.el --- Music Player Configuration       -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Charles Choi

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

(require 'now-playing)
(require 'shazam)
(require 'triode)

(defcustom cc-music-player-binding-desktop "<f14>"
  "Primary key binding for music player."
  :type 'string
  :group 'kickingvegas)

(defalias 'np 'now-playing-tmenu "Alias to `now-playing-tmenu'.")

(defun cc/music-init (&optional a b)
  "Initialize music players with bindings A and B."
  (now-playing-init a)
  (triode-init b))

(defun cc/music-swap-player ()
  "Swap music player."
  (interactive)
  (let* ((b "<f14>")
         (current-player (key-binding (kbd b))))

    (cond
     ((eq current-player #'now-playing-tmenu)
      (keymap-global-set b #'triode-tmenu)
      (message "Set %s to Triode" b))

     ((eq current-player #'triode-tmenu)
      (keymap-global-set b #'now-playing-tmenu)
      (message "Set %s to Now Playing" b))

     (t
      (keymap-global-set b #'now-playing-tmenu)
      (message "Set %s to Now Playing" b)))))

(cc/music-init "H-[" "H-]")

(keymap-global-set "<f14>" #'triode-tmenu)

(keymap-global-set "H-<f14>" #'cc/music-swap-player)

(shazam-init "s-<f5>")
(keymap-global-set "H-<f5>" #'shazam-history)

(provide 'cc-music)
;;; cc-music.el ends here
