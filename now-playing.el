;;; now-playing.el --- macOS Music Player Interface  -*- lexical-binding: t; -*-

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
(require 'transient)



;;; Variables and Constants

(defvar np--poll-timer nil
  "Timer for polling current track.")

(defvar np--volume nil
  "Music app volume.")

(defvar np--current-track ""
  "Music app current track.")

(defcustom np-volume-delta
  5
  "Change increment for sound volume.
This value is the amount of change that will be applied to
`now-playing--volume' by the commands `now-playing-increase-volume' and
`now-playing-decrease-volume'."
  :type 'integer
  :group 'kickingvegas)

(defconst np--osascript-music-init '("tell" "application" "\"Music\"" "to")
  "Music app osascript command initializer.")


;;; Functions

(defun np-osascript (arg)
  "Run osascript with ARG."
  (interactive "sOSAScript: ")
  (np--osascript arg))

(defun np--osascript (arg)
  "Process ARG with OSAscript."
  (process-lines "osascript" "-e" arg))

(defun np--run-clause (clause)
  "Execute CLAUSE using `process-lines'."
  (let* ((cmdlist (append np--osascript-music-init clause))
         (cmd (string-join cmdlist " ")))
    (process-lines "osascript" "-e" cmd)))

(defun np--run-clause-native (clause)
  "Execute CLAUSE using `ns-do-applescript'."
  (let* ((cmdlist (append np--osascript-music-init clause))
         (cmd (string-join cmdlist " ")))
    (list (ns-do-applescript cmd))))

(defun np-playpause ()
  "Play or pause Music app."
  (interactive)
  (let ((clause '("playpause")))
    (np-get-volume)
    (np--run-clause-native clause)))

(defun np-stop ()
  "Stop Music app."
  (interactive)
  (let ((clause '("stop")))
    (np-get-volume)
    (np--run-clause-native clause)))

(defun np-next-track ()
  "Next track Music app."
  (interactive)
  (let ((clause '("next" "track")))
    (np--run-clause-native clause)))

(defun np-previous-track ()
  "Previous track Music app."
  (interactive)
  (let ((clause '("previous" "track")))
    (np--run-clause-native clause)))

(defun np-get-volume ()
  "Get Music app sound volume."
  (interactive)
  (let* ((clause '("get" "sound" "volume"))
         (result (np--run-clause-native clause))
         (volume (car result))
         ;; (volume (if (> (length result) 0)
         ;;             (string-to-number (car result))))
         )
    (setq np--volume volume)
    volume))

(defun np-set-volume (arg)
  "Set Music app sound volume to ARG."
  (interactive "nSet Volume (0-100): ")
  (let* ((clause '("set" "sound" "volume" "to"))
         (clause (append clause (list (number-to-string arg)))))
    (setq np--volume arg)
    (np--run-clause-native clause)
    (message "Sound Volume: %d" arg)))

(defun np-increase-volume ()
  "Increase Music app sound volume."
  (interactive)
  (if (not np--volume)
      (np-get-volume))
  (let* ((new-volume (+ np--volume np-volume-delta))
         (volume (cond
                  ((<= new-volume 100) new-volume)
                  ((> new-volume 100) 100)))
         (clause (list "set" "sound" "volume" "to"
                       (number-to-string volume))))
    (np-set-volume volume)
    (np--run-clause-native clause)))

(defun np-decrease-volume ()
  "Decrease Music app sound volume."
  (interactive)
  (if (not np--volume)
      (np-get-volume))
  (let* ((new-volume (- np--volume np-volume-delta))
         (volume (cond
                  ((>= new-volume 0) new-volume)
                  ((< new-volume 0) 0)))
         (clause (list "set" "sound" "volume" "to"
                       (number-to-string volume))))
    (np-set-volume volume)
    (np--run-clause-native clause)))

(defun np--current-track ()
  "Get current track on Music app."
  (let* ((clause '("if"
                   "((player" "state" "is" "playing)" "or"
                   "(player" "state" "is" "paused))"
                   "then" "name" "of" "current" "track"
                   "&" "\" • \""
                   "&" "artist" "of" "current" "track"
                   "&" "\" • \""
                   "&" "album" "of" "current" "track"))
         (result (car (np--run-clause-native clause))))
    result))

(defun np-current-track ()
  "Get current track on Music app."
  (interactive)
  (let ((track (np--current-track)))
    (if track
        (message "Now Playing: %s" track)
      (message "No track playing"))))


(defun np--player-state ()
  "Get Music app player state."
  (let* ((clause '("get" "player" "state"))
         (result (car (np--run-clause clause))))
    result))

(defun np-launch-music ()
  "Launch Music app."
  (interactive)
  (process-lines "open" "-a" "Music"))

(defun np--tmenu-refresh ()
  "Refresh menu."
  (interactive)
  (transient--show))



;;; Polling

(defun np-current-track-log ()
  "Get current track on Music app."
  (interactive)
  (let* ((track (np--current-track))
         (ts (format-time-string "[%Y-%m-%d %H:%M:%S %Z]"))
         (msg (format "%s Now Playing: %s" ts track))
         (buf (get-buffer-create "*now playing log*")))
    (when track
      (message msg)
      (unless (string-equal track np--current-track)
        (setq np--current-track track)
        (with-current-buffer buf
          (setq buffer-read-only t)
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert (concat msg "\n"))))))))

(defun np-is-logging-p ()
  "Predicate if logging the current track."
  (if np--poll-timer
      t
    nil))

(defun np-find-log ()
  "Find *now playing log* buffer."
  (interactive)
  (let ((buf (get-buffer "*now playing log*")))
    (if buf
        (progn
          (switch-to-buffer buf)
          (setq buffer-read-only t))
      (message "No *now playing log* buffer"))))

(defun np-start-polling-current-track ()
  "Poll current track every 5 minutes."
  (interactive)
  (if (np-is-logging-p)
      (setq np--poll-timer (run-at-time nil 200 #'np-current-track-log))
    (message "Already polling current track")))

(defun np-cancel-poll ()
  "Cancel poll timer."
  (interactive)
  (if (not np--poll-timer)
      (message "Not polling current track")
    (cancel-timer np--poll-timer)
    (setq np--poll-timer nil)
    (message "Cancelled track polling")))


;;; Transient

(transient-define-prefix np-tmenu ()
  "Now playing Transient menu for macOS Music app."
  :refresh-suffixes t
  ["Now Playing"
   :class transient-row
   :description (lambda () (format "Now Playing: %s"
                              (let ((track (np--current-track)))
                                (if track
                                    track
                                  ""))))
   ("p" "⏮" np-previous-track :transient t)
   ("SPC" "Play/Pause" np-playpause
    :description (lambda ()
                   (let* ((state (np--player-state)))
                     (cond
                      ((string-equal state "playing") "⏸")
                      ((string-equal state "paused") "▶")
                      ((string-equal state "stopped") "▶")
                      (t "Unknown"))))
    :transient t)

   ("s" "⏹" np-stop :transient t)
   ("n" "⏭" np-next-track :transient t)
   ("<up>" "+" np-increase-volume :transient t)
   ("<down>" "−" np-decrease-volume :transient t)
   ("r" "⟲" np--tmenu-refresh :transient t)
   ("o" "♫" np-launch-music)
   ("RET" "Dismiss" transient-quit-all)])

(provide 'now-playing)
;;; now-playing.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("np-" . "now-playing-"))
;; End:
