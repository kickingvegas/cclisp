;;; triode.el --- Triode Interface                   -*- lexical-binding: t; -*-

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

;; Emacs interface to Triode app (https://triode.app/) via Shortcuts.

;;

;;; Code:
(require 'map)
(require 'transient)
(require 'shazam)

(defgroup triode nil
  "Group for Triode settings."
  :group 'convenience)

(defcustom triode-stations '("WTJU" "BFF.fm" "BBC Radio 1Xtra"
                             "BBC Radio 1" "KALW" "KCRW Music")
  "List of radio stations."
  :type '(repeat string)
  :group 'triode)

(defcustom triode-dismiss-menu-for-actions
  nil
  "If non-nil then dismiss `triode-tmenu' for action commands."
  :type 'boolean
  :group 'triode)

(defvar triode--shortcut-template "shortcuts run 'Triode %s' | cat"
  "Shortcut template for Triode.")

(defvar triode-is-muting nil
  "If non-nil, then muting is on.

Note this value is inferred.")

(defvar triode-is-playing nil
  "If non-nil, then Triode is playing.

Note this value is inferred.")

(defvar triode-current-station ""
  "Current station.")

(defvar triode-station-db (make-hash-table :test #'equal)
  "Station database.")

(defun triode--dismiss-menu-for-actions ()
  "Transient state function based on `triode-dismiss-menu-for-actions'."
  (if triode-dismiss-menu-for-actions
      (transient--do-return)
    (transient--do-stay)))


(defun triode--make-request (clause &optional clip)
  "Make request to Triode with CLAUSE.

If CLIP is non-nil, then store result in `kill-ring'."
  (let* ((request (format triode--shortcut-template clause))
         (response (shell-command-to-string request)))
    (if clip
        (kill-new response))
    response))

(defun triode-current-state ()
  "Get current state of Triode."
  (interactive)
  (let* ((response (triode--make-request "Now Playing JSON"))
         (jsondb (json-parse-string response :null-object nil))
         (playback-state (map-elt jsondb "playbackState"))
         (track (map-elt jsondb "track"))
         (artist (map-elt jsondb "artist"))
         (album (map-elt jsondb "album"))
         ;; (station-id (map-elt jsondb "stationID"))
         )

    (if (string-equal playback-state "Playing")
        (setq triode-is-playing t)
      (setq triode-is-playing nil))

    (if (and (stringp track) (string-equal track ""))
        (map-put! jsondb "track" nil))

    (if (and (stringp artist) (string-equal artist ""))
        (map-put! jsondb "artist" nil))

    (if (and (stringp album) (string-equal album ""))
        (map-put! jsondb "album" nil))

    jsondb))

(defun triode-now-playing ()
  "Get what is now playing on Triode."
  (interactive)
  (let* ((current-state (triode-current-state))
         (msg (triode--tmenu-description current-state)))
    (kill-new msg)
    (message "%s" msg)))

(defun triode-play ()
  "Play Triode."
  (interactive)
  (setq triode-is-playing t)
  (triode--make-request "Start"))

(defun triode-stop ()
  "Stop Triode."
  (interactive)
  (setq triode-is-playing nil)
  (triode--make-request "Stop"))

(defun triode-mute ()
  "Mute Triode."
  (interactive)
  (setq triode-is-muting t)
  (triode--make-request "Mute On"))

(defun triode-unmute ()
  "Unmute Triode."
  (interactive)
  (setq triode-is-muting nil)
  (triode--make-request "Mute Off"))

(defun triode-station-gui ()
  "Choose station using Triode GUI."
  (interactive)
  (let ((result (triode--make-request "Station JSON")))
    (when (not (string-search "Error" result))
      (let* ((response (json-parse-string result :null-object nil))
             (name (substring-no-properties (map-elt response "name")))
             (station-id (map-elt response "stationID")))

        (unless (map-contains-key triode-station-db station-id)
          (map-put! triode-station-db station-id name))

        (setq triode-is-playing t)

        (setq triode-current-station name)))))

(defun triode-station ()
  "Open station."
  (interactive)
  (let* ((choice (completing-read "Station: " triode-stations))
         (station (format "Play %s" choice)))
    (setq triode-current-station choice)
    (setq triode-is-playing t)
    (triode--make-request station)))

(defun triode-launch ()
  "Launch Triode app."
  (interactive)
  (process-lines "open" "-a" "Triode"))

(defun triode--tmenu-refresh ()
  "Refresh menu."
  (interactive)
  (transient--show))

(defun triode-customize-group ()
  "Customize ‘triode’ group."
  (interactive)
  (customize-group "triode"))

(defun triode-init (&optional b)
  "Initialize Triode, binding B to `triode-tmenu'.

If B is not defined, then the binding <f14> we be used by default."
  (interactive)
  (let ((b (if (not b) "<f14>" b)))
    (if (not (eq system-type 'darwin))
        (error "Only supported on macOS")
      (if (and (display-graphic-p) (fboundp 'set-fontset-font))
          (set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display"))
      (keymap-global-set b #'triode-tmenu))))


(defun triode--tmenu-description (current-state)
  "Render description given CURRENT-STATE."
  (let* ((playback-state (map-elt current-state "playbackState"))
         (track (map-elt current-state "track"))
         (artist (map-elt current-state "artist"))
         (album (map-elt current-state "album"))
         (station-id (map-elt current-state "stationID"))
         (station (map-elt triode-station-db station-id triode-current-station)))

    (setq triode-is-playing (string-equal playback-state "Playing"))
    (cond
     ((and station track artist album)
      (format "[%s] %s • %s • %s"
              station
              track
              artist
              album))

     ((and station track artist)
      (format "[%s] %s • %s"
              station
              track
              artist))

     ((and station track)
      (format "[%s] %s"
              station
              track))

     (station
      ;; (unless (string-equal triode-current-station track)
      ;;   (setq triode-current-station track))
      (format "[%s]" station))

     (t
      (format "[%s]" triode-current-station)))))

(transient-define-prefix triode-tmenu ()
  "Transient menu for Triode app."
  :refresh-suffixes t

  ["Triode"
   :class transient-row
   :description (lambda () (triode--tmenu-description (triode-current-state)))
   ("s" "􀪔…" triode-station-gui)
   ("SPC" "􀊄" triode-play
    :transient triode--dismiss-menu-for-actions
    :if-not (lambda () triode-is-playing))
   ("SPC" "􀛷" triode-stop
    :transient triode--dismiss-menu-for-actions
    :if (lambda () triode-is-playing))
   ("m" "􀊢" triode-mute
    :transient triode--dismiss-menu-for-actions
    :if-not (lambda () triode-is-muting))
   ("m" "􀊣" triode-unmute
    :transient triode--dismiss-menu-for-actions
    :if (lambda () triode-is-muting))
   ("r" "􀅈" triode--tmenu-refresh :transient t)
   ("z" "􁈴" shazam)
   ("o" "􀑪" triode-launch)
   ("," "􀣋" triode-customize-group)
   ("RET" "􀀲" transient-quit-all)])

(provide 'triode)
;;; triode.el ends here
