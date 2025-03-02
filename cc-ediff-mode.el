;;; cc-ediff-mode.el --- Ediff configuration -*- lexical-binding: t; -*-

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

(require 'ediff)
(require 'casual-lib)
;;; Code:

;; these defvars are here to let cc-ediff-mode.el compile clean
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-merge-job)
(defvar ediff-ancestor-buffer)

;; CC: I set my Ediff variables in `custom-set-variables'
;; Use your own preference.
;; '(ediff-keep-variants nil)
;; '(ediff-split-window-function 'split-window-horizontally)
;; '(ediff-window-setup-function 'ediff-setup-windows-plain)

(defvar cc/ediff-revision-session-p nil
  "If t then `cc/ediff-revision-actual' has been called.
This state variable is used to insert added behavior to the overridden
function `ediff-janitor'.")

(defun cc/ediff-revision-from-menu (e)
  "Invoke `ediff-revision' on E with variable `buffer-file-name'."
  (interactive "e")
  (ignore e)
  (cc/ediff-revision))

(defun cc/ediff-revision ()
  "Run Ediff on the current `buffer-file-name' provided that it is `vc-registered'.
This function handles the interactive concerns found in `ediff-revision'.
This function will also test if a diff should apply to the current buffer."
  (interactive)
  (when (and (bound-and-true-p buffer-file-name)
             (vc-registered (buffer-file-name)))
    (if (and (buffer-modified-p)
	     (y-or-n-p (format "Buffer %s is modified.  Save buffer? "
                               (buffer-name))))
      (save-buffer (current-buffer)))
    (message buffer-file-name)
    (cc/ediff-revision-actual))

  (cond ((not (bound-and-true-p buffer-file-name))
         (message (concat (buffer-name) " is not a file that can be diffed.")))
        ((not (vc-registered buffer-file-name))
         (message (concat buffer-file-name " is not under version control.")))))

(defun cc/ediff-revision-actual ()
  "Invoke Ediff logic to diff the modified repo file to its counterpart in the
current branch.
This function handles the actual diff behavior called by `ediff-revision'."
  (let ((rev1 "")
        (rev2 ""))
    (setq cc/ediff-revision-session-p t)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 nil)))

(defun cc/ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according to
`ediff-quit-merge-hook'.
ASK non-nil means ask the user whether to keep each unmodified buffer, unless
KEEP-VARIANTS is non-nil, in which case buffers are never killed.
A side effect of cleaning up may be that you should be careful when comparing
the same buffer in two separate Ediff sessions: quitting one of them might
delete this buffer in another session as well.

CC MODIFICATION: This method overrides the original Ediff function."
  (let ((ask (if (and (boundp 'cc/ediff-revision-session-p)
                      cc/ediff-revision-session-p)
                 nil
               ask)))
    (ediff-dispose-of-variant-according-to-user
     ediff-buffer-A 'A ask keep-variants)
    ;; !!!: CC Note: Test global state variable `cc/ediff-revision-session-p' to
    ;; determine if the modified repo file should be kept.
    ;; Guarding in place to hopefully avoid side-effects when `ediff-janitor' is
    ;; called from other Ediff functions. Informal testing has not revealed any
    ;; side-effects but YOLO.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (ediff-dispose-of-variant-according-to-user
         ;; CC Note: keep-variants argument is hard-coded to t to keep
         ;; buffer holding modified repo file around.
         ediff-buffer-B 'B t t)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-B 'B ask keep-variants))
    (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
        (ediff-dispose-of-variant-according-to-user
         ediff-ancestor-buffer 'Ancestor ask keep-variants)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-C 'C ask keep-variants))
    ;; CC Note: Reset global state variable `cc/ediff-revision-session-p'.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (setq cc/ediff-revision-session-p nil))))

(advice-add 'ediff-janitor :override #'cc/ediff-janitor)
;; (advice-remove 'ediff-janitor #'cc/ediff-janitor)

(defun cc/stash-window-configuration-for-ediff ()
  "Store window configuration to register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (window-configuration-to-register ?🧊))

(defun cc/restore-window-configuration-for-ediff ()
  "Restore window configuration from register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (jump-to-register ?🧊))

(add-hook 'ediff-before-setup-hook #'cc/stash-window-configuration-for-ediff)
;; !!!: CC Note: Why this is not `ediff-quit-hook' I do not know. But this works
;; for cleaning up ancillary buffers on quitting an Ediff session.
(add-hook 'ediff-after-quit-hook-internal #'cc/restore-window-configuration-for-ediff)


(transient-define-prefix casual-ediff-tmenu ()
  :refresh-suffixes t
  ["Casual Ediff"
   ["A"
    :if (lambda () (and ediff-buffer-A) (not ediff-buffer-C))
    :description (lambda () (format "A: %s" (buffer-name ediff-buffer-A)))
    ("a" "A→B" ediff-copy-A-to-B
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-B)))
    ("ra" "Restore A"
     (lambda ()
       (interactive)
       (casual-ediff-restore-diff ?a)
       (casual-ediff-save-buffer ?a))
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-A))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-A)))]

   ["A"
    :if (lambda () (and ediff-buffer-A ediff-buffer-C))
    :description (lambda () (format "A: %s" (buffer-name ediff-buffer-A)))
    ("ac" "A→C" ediff-copy-A-to-C
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-C)))
    ("ra" "Restore A"
     (lambda ()
       (interactive)
       (casual-ediff-restore-diff ?a)
       (casual-ediff-save-buffer ?a))
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-A))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-A)))]


   ["Diff"
    :if (lambda () (not ediff-buffer-C))
    ("p" "↑" ediff-previous-difference :transient t)
    ("n" "↓" ediff-next-difference :transient t)
    ("!" "⟲" ediff-update-diffs :transient t)
    ("|" "H/V" ediff-toggle-split
     :transient t
     :description (lambda ()
                    (if (eq ediff-split-window-function 'split-window-vertically)
                        "Split ――"
                      "Split |")))
    ("#" "Skip 𝑤𝑠" ediff-toggle-skip-similar :transient t
     :description (lambda ()
                    (if ediff-ignore-similar-regions
                        "Include 𝑤𝑠"
                      "Skip 𝑤𝑠")))]

   ["B"
    :if (lambda () (and ediff-buffer-B (not ediff-buffer-C)))
    :description (lambda () (format "B: %s" (buffer-name ediff-buffer-B)))
    ("b" "A←B" ediff-copy-B-to-A :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-A)))

    ;; ("bc" "B→C" ediff-copy-B-to-C :transient t
    ;;  :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-C)))

    ("wb" "Save B"
     (lambda ()
       (interactive)
       (casual-ediff-save-buffer ?b))
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-B))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-B)))

    ("rb" "Restore B"
     (lambda ()
       (interactive)
       (casual-ediff-restore-diff ?b)
       (casual-ediff-save-buffer ?b))
     :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-B))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-B)))]


   ["B"
    :if (lambda () (if (and ediff-buffer-B ediff-buffer-C) t nil))
    :description (lambda () (format "B: %s" (buffer-name ediff-buffer-B)))
    ("ba" "A←B" ediff-copy-B-to-A :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-A)))

    ("bc" "B→C" ediff-copy-B-to-C :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-C)))

    ("wb" "Save B"
     (lambda ()
       (interactive)
       (casual-ediff-save-buffer ?b))
     :transient t
     :if (lambda ()
           (if (string= (file-name-extension (buffer-name ediff-buffer-B)) "~{index}")
               nil
             (if (not (casual-ediff-buffer-read-only-p ediff-buffer-B))
                 t
               nil)))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-B)))

    ("rb" "Restore B"
     (lambda ()
       (interactive)
       (casual-ediff-restore-diff ?b)
       (casual-ediff-save-buffer ?b))
     :transient t
     :if (lambda ()
           (if (string= (file-name-extension (buffer-name ediff-buffer-B)) "~{index}")
               nil
             (if (not (casual-ediff-buffer-read-only-p ediff-buffer-B))
                 t
               nil)))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-B)))]

   ["Diff"
    :if (lambda () (and ediff-buffer-C t))
    ("p" "↑" ediff-previous-difference :transient t)
    ("n" "↓" ediff-next-difference :transient t)
    ("!" "⟲" ediff-update-diffs :transient t)
    ("|" "H/V" ediff-toggle-split
     :description (lambda ()
                    (if (eq ediff-split-window-function 'split-window-vertically)
                        "Split ――"
                      "Split |"))
     :transient t)]

   ["C"
    :if (lambda () (if ediff-buffer-C t nil))
    :description (lambda ()
                   (if ediff-buffer-C
                       (format "C: %s" (buffer-name ediff-buffer-C))
                     ))

    ("cb" "B←C" ediff-copy-C-to-B :transient t
     :if (lambda ()
           (if (not (casual-ediff-buffer-read-only-p ediff-buffer-B))
               (if (string= (file-name-extension (buffer-name ediff-buffer-B)) "~{index}")
                   nil
                 t)
             nil)))

    ("ca" "A←C" ediff-copy-C-to-A :transient t
     :if-not (lambda () (casual-ediff-buffer-read-only-p ediff-buffer-A)))

    ("rc" "Restore C"
     (lambda ()
       (interactive)
       (casual-ediff-restore-diff ?c)
       (casual-ediff-save-buffer ?c))
     :transient t
     :if (lambda ()
           (if (not (casual-ediff-buffer-read-only-p ediff-buffer-C))
               (if (not (string= (buffer-name ediff-buffer-C) "*ediff-merge*"))
                   t
                 nil)
             nil))
     :inapt-if-not (lambda () (buffer-modified-p ediff-buffer-C)))

    ;; Note: Ediff doesn't support combining merge conflicts from both. The
    ;; workaround is to directly edit the C buffer.

    ;; TODO: The buffer *ediff-merge* is already modified. How can you tell if it needs to be restored?

    ("r" "Restore Merge"
     (lambda ()
       (interactive)
       (ediff-restore-diff-in-merge-buffer nil))
     :transient t
     :if (lambda ()
           (if (not (casual-ediff-buffer-read-only-p ediff-buffer-C))
               (if (string= (buffer-name ediff-buffer-C) "*ediff-merge*")
                   t
                 nil)
             nil)))]]

  [:class transient-row
   (casual-lib-quit-one)
   ("q" "Quit Ediff" ediff-quit)])

(defun casual-ediff-restore-diff (key)
  (interactive)
  (ediff-restore-diff nil key))

(defun casual-ediff-save-buffer (key)
  (interactive)
  (setq last-command-event key)
  (ediff-save-buffer nil))

(defun casual-ediff-buffer-read-only-p (buf)
  (with-current-buffer buf
    (if buffer-read-only t nil)))

(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (keymap-set ediff-mode-map "C-o" #'casual-ediff-tmenu)))

(provide 'cc-ediff-mode)

;;; cc-ediff-mode.el ends here
