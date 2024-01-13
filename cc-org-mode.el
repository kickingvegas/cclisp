;;; cc-org-mode.el --- Org configuration -*- lexical-binding: t; -*-

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
(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'doct)
(require 'org-superstar)
(require 'face-remap)
(require 'cclisp)
(require 'cc-save-hooks)
(require 'company)
(require 'hl-line)
(if (eq system-type 'darwin)
    (require 'ob-swiftui))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; default for dev5
(defvar cc/org-daily-header-template "CC Notes - %a %b %d %Y"
  "Custom daily Org header template.")

(defun cc/--current-org-default-notes-file ()
  "String path to current daily Org file.
This function is dependent upon this file being created by a daily cron job."
  (format-time-string "~/org/%Y_%m_%d.org"))

(defun cc/--find-capture-point-in-file (key)
  "Move point to the end of the first instance of KEY in the current buffer."
  (goto-char (point-min))
  (search-forward key))

(defun cc/--find-capture-point-in-current ()
  "Helper function to locate where to insert capture item in daily Org file.
This function is intended to be passed into `doct' via the :function property."
  (cc/--find-capture-point-in-file
   (format-time-string cc/org-daily-header-template)))

;; Configure org-capture-templates using doct
(setq org-capture-templates
      (doct '(("Appointment"
               :keys "a"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* %^{description}"
                          "%^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?"))

              ("BeOrg TODO"
               :keys "b"
               :type entry
               :file "~/org/refile-beorg.org"
               :empty-lines 1
               :template ("* TODO %^{description}"
                          "SCHEDULED: %^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?"))

              ("TODO: Scheduled"
               :keys "s"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* TODO %^{description} %^G"
                          "SCHEDULED: %^T"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?"))

              ("TODO: Unscheduled"
               :keys "t"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* TODO %^{description} %^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?"))

              ("TODO: Blog Post"
               :keys "p"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* TODO Post: %^{description} :blog%^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%?"))

              ("TODO: Issue"
               :keys "i"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* TODO %^{description} %^G"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          ""
                          "** Title"
                          "%?"
                          "** Description\n"
                          "** Environment\n"
                          "** Steps to Reproduce\n"
                          "** Expected Result\n"
                          "** Actual Result\n"))

              ("Journal"
               :keys "j"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("%(datestamp2)"
                          "%?"))

              ("Captee Capture"
               :keys "c"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template ("* %:description"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":END:"
                          "%:annotation"
                          "%i"
                          ""
                          "%?"))

              ("WWDC Capture"
               :keys "w"
               :type entry
               :file "~/org/wwdc23.org"
               :empty-lines 1
               :template ("** TODO %:description"
                          "%:annotation"
                          "%i"
                          "%?"))

              ("Song"
               :keys "o"
               :type entry
               :file "~/org/songs/songs.org"
               :headline "Songs"
               :prepend t
               :template ("* %^{Song}"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":ARTIST: %^{Artist}"
                          ":END:"
                          "%?")))))

(setq org-todo-keywords
           '((sequence "TODO(t)" "IN_PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
             (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
        ("IN_PROGRESS" . "dark orange")
        ("WAITING" . "dark orange")
        ("DONE" . "sea green")
        ("CANCELED" . (:foreground "blue" :weight bold))))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-log-done 'time)

(add-hook 'org-mode-hook 'org-superstar-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-clock-persistence-insinuate)
(add-hook 'org-mode-hook #'cc/save-hook-delete-trailing-whitespace)

(add-hook
 'org-mode-hook
 (lambda ()
   (add-to-list (make-local-variable 'company-backends)
                'company-org-block)))

(define-key org-mode-map (kbd "M-<f8>") 'datestamp)
;; (define-key org-mode-map (kbd "<f9>") 'avy-goto-word-1)
(define-key org-mode-map (kbd "M-<f6>") 'org-toggle-inline-images)
(define-key org-mode-map (kbd "C-c t") 'cc/org-time-stamp-inactive)
(define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
(define-key org-mode-map (kbd "<end>") 'org-end-of-line)
(define-key org-mode-map (kbd "A-<left>") 'org-backward-sentence)
(define-key org-mode-map (kbd "A-<right>") 'org-forward-sentence)
(define-key org-mode-map (kbd "A-M-<left>") 'org-backward-paragraph)
(define-key org-mode-map (kbd "A-M-<right>") 'org-forward-paragraph)
(define-key org-mode-map (kbd "C-<up>") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "C-<down>") 'org-next-visible-heading)
(define-key org-mode-map (kbd "M-v") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "M-j") 'cc/journal-entry)
(define-key org-mode-map (kbd "C-v") 'org-next-visible-heading)
(define-key org-mode-map (kbd "C-/") 'org-emphasize)

(add-hook 'org-agenda-finalize-hook 'hl-line-mode)
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (define-key org-agenda-mode-map
              [(double-mouse-1)] 'org-agenda-goto-mouse)))

(defun cc/org-agenda-goto-now ()
  "Redo agenda view and move point to current time '← now'."
  (interactive)
  (org-agenda-redo)
  (org-agenda-goto-today)
  (search-forward " now "))

(define-key org-agenda-mode-map (kbd "<f1>") 'org-save-all-org-buffers)
(define-key org-agenda-mode-map (kbd "M-p") 'org-agenda-previous-date-line)
(define-key org-agenda-mode-map (kbd "M-n") 'org-agenda-next-date-line)
(define-key org-agenda-mode-map (kbd ".") 'cc/org-agenda-goto-now)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   (sql . t)
   (sqlite . t)
   (restclient . t)
   (plantuml . t)
   (swift . t)))

(when (fboundp 'ob-swiftui-setup)
  (ob-swiftui-setup))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; (define-key org-mode-map
;;   [menu-bar table table-field-info]
;;   '("Table Field Info" . org-table-field-info))


(require 'ox-publish)

(setq org-publish-project-alist
      `(("pages"
         :base-directory "~/Projects/Captee/Development/Captee/docs/help"
         :base-extension "org"
         :recursive t
         :publishing-directory "~/Projects/Captee/Development/Captee/Captee.help/Contents/Resources/en.lproj"
         :publishing-function org-html-publish-to-html)

        ("static"
         :base-directory "~/Projects/Captee/Development/Captee/docs/help"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png\\|svg\\|helpindex\\|cshelpindex"
         :recursive t
         :publishing-directory "~/Projects/Captee/Development/Captee/Captee.help/Contents/Resources/en.lproj"
         :publishing-function org-publish-attachment)

        ("captee-help-book"
         :components ("pages" "static"))))

;; ox-gfm init is so broken. need to load it manually.
;; (eval-after-load "org"
;;   '(require 'ox-gfm nil t))

;; (use-package ox-gfm
;;   :defer 3
;;   :after org)

(defun cc/journal-entry ()
  "Capture journal entry in Org."
  (interactive)
  (org-capture nil "j"))

(provide 'cc-org-mode)
;;; cc-org-mode.el ends here
