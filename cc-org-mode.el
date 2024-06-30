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
(require 'org-ql)
(require 'cclisp)
(require 'cc-save-hooks)
(require 'company)
(require 'hl-line)
(require 'prog-mode)
(require 'cc-org-smart-quotes)

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

(defun cc/org-checkbox-in-progress ()
  "If point is on an Org list item, set it to be a checkbox in-progress."
  (interactive)
  (org-ctrl-c-ctrl-c '(16)))

(defun cc/org-toggle-list-is-checkbox ()
  "If point is on an Org list item, toggle if the list item is also a checkbox.
Note that this function does not toggle the actual value of a checkbox,
which is done with `org-ctrl-c-ctrl-c'."
  (interactive)
  (org-ctrl-c-ctrl-c '(4)))

(defun cc/config-capture-template (&optional prefix suffix)
  (let* ((properties (list ":PROPERTIES:"
                           ":CREATED: %U"
                           ":END:"))
         (properties (if prefix (append prefix properties) properties))
         (properties (if suffix (append properties suffix) properties)))
    properties))

;; Configure org-capture-templates using doct
(setq org-capture-templates
      (doct '(("Appointment"
               :keys "a"
               :type entry
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :empty-lines 1
               :template (lambda ()
                           (cc/config-capture-template '("* %^{description}"
                                                         "%^T")
                                                       '("%?"))))

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

              ("TODO"
               :keys "t"
               :type entry
               :empty-lines 1
               :file cc/--current-org-default-notes-file
               :function cc/--find-capture-point-in-current
               :children (("Scheduled"
                           :keys "s"
                           :todo-state "TODO"
                           :template (lambda ()
                                       (cc/config-capture-template
                                        '("* %{todo-state} %^{description} %^G"
                                          "SCHEDULED: %^T")
                                        '("%?"))))

                          ("Unscheduled"
                           :keys "t"
                           :todo-state "TODO"
                           :template (lambda ()
                                       (cc/config-capture-template
                                        '("* %{todo-state} %^{description} %^G")
                                        '("%?"))))

                          ("Blog Post"
                           :keys "p"
                           :todo-state "TODO"
                           :template (lambda ()
                                       (cc/config-capture-template
                                        '("* %{todo-state} Post: %^{description} :blog%^G")
                                        '("%?"))))

                          ("Issue"
                           :keys "i"
                           :todo-state "TODO"
                           :template (lambda ()
                                       (cc/config-capture-template
                                        '("* %{todo-state} %^{description} %^G")
                                        '("\n** Title"
                                          "%?"
                                          "** Description\n"
                                          "** Environment\n"
                                          "** Steps to Reproduce\n"
                                          "** Expected Result\n"
                                          "** Actual Result\n"
                                          ))))))

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
               :file "~/org/wwdc24.org"
               :headline "WWDC 24 Notes"
               :empty-lines 1
               :template ("* TODO %:description"
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
;;(add-hook 'org-mode-hook #'cc/save-hook-delete-trailing-whitespace)
(add-hook 'org-mode-hook (lambda ()
                           (cc/reconfig-org-smart-quotes-lang "en")))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-to-list (make-local-variable 'company-backends)
                'company-org-block)))

(defun cc/--prettify-components (prefix suffix)
  "Generate a components argument for `prettify-symbols-alist'.
PREFIX - character to use first
SUFFIX - string appended to prefix
\nRefer to `reference-point-alist' for more information on Br and Bl."
  (let ((result (list prefix))
        (suffix-list (mapcar (lambda (c) (char-to-string c)) suffix)))
    (mapc (lambda (x)
            (push '(Br . Bl) result)
            (push (string-to-char x) result))
          suffix-list)
    (reverse result)))

(when (display-graphic-p)
  (add-hook
   'org-mode-hook
   (lambda ()
     "Prettify Org keywords."
     (let* ((cc-temp-list '(("#+results:" . ?∴ )
                            (":properties:" . ?⚙ )
                            (":end:" . ?🔚 )
                            (":logbook:" . ?📓 )
                            ("[ ]" .  ?☐ )
                            ("[x]" . ?☑ )
                            ("[-]" . ?✈ ))))
       (dolist (e cc-temp-list)
         (push e prettify-symbols-alist)
         (push (list (upcase (car e)) (nthcdr 1 e)) prettify-symbols-alist)))

     (let* ((base-list (list "center"
                             "comment"
                             "example"
                             "export"
                             "quote"
                             "src"
                             "verse")))
       (dolist (e base-list)
         (push (cons (concat "#+begin_" e)
                     (cc/--prettify-components ?⎧ e)) prettify-symbols-alist)
         (push (cons (concat "#+BEGIN_" (upcase e))
                     (cc/--prettify-components ?⎧ e)) prettify-symbols-alist)
         (push (cons (concat "#+end_" e)
                     (cc/--prettify-components ?⎩ e)) prettify-symbols-alist)
         (push (cons (concat "#+END_" (upcase e))
                     (cc/--prettify-components ?⎩ e)) prettify-symbols-alist)))
     (prettify-symbols-mode))))

(defun cc/org-backward-paragraph ()
  "Move point backward an Org paragraph such that the first line is highlighted."
  (interactive)
  (org-backward-paragraph 2)
  (forward-line))

(defun cc/org-forward-paragraph ()
  "Move point forward an Org paragraph such that the first line is highlighted."
  (interactive)
  (org-forward-paragraph)
  (forward-line))

(keymap-set org-mode-map "M-<f8>" #'datestamp)
;; (keymap-set org-mode-map "<f9>" 'avy-goto-word-1)
(keymap-set org-mode-map "M-<f9>" #'cc/org-checkbox-in-progress)
(keymap-set org-mode-map "C-<f9>" #'cc/org-toggle-list-is-checkbox)
(keymap-set org-mode-map "<f9>" #'org-ctrl-c-ctrl-c)
(keymap-set org-mode-map "M-<f6>" #'org-toggle-inline-images)
(keymap-set org-mode-map "C-c t" #'cc/org-time-stamp-inactive)
(keymap-set org-mode-map "C-<home>" #'org-beginning-of-line)
(keymap-set org-mode-map "C-<end>" #'org-end-of-line)
(keymap-set org-mode-map "<home>" #'beginning-of-buffer)
(keymap-set org-mode-map "<end>" #'end-of-buffer)
(keymap-set org-mode-map "A-<left>" #'org-backward-sentence)
(keymap-set org-mode-map "A-<right>" #'org-forward-sentence)
(keymap-set org-mode-map "M-p" #'cc/org-backward-paragraph)
(keymap-set org-mode-map "M-n" #'cc/org-forward-paragraph)
(keymap-set org-mode-map "C-<up>" #'org-previous-visible-heading)
(keymap-set org-mode-map "C-<down>" #'org-next-visible-heading)
(keymap-set org-mode-map "M-v" #'org-previous-visible-heading)
(keymap-set org-mode-map "M-j" #'cc/journal-entry)
(keymap-set org-mode-map "C-v" #'org-next-visible-heading)
(keymap-set org-mode-map "C-/" #'org-emphasize)
(keymap-set org-mode-map "C-6" #'org-goto)

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

(keymap-set org-agenda-mode-map "<f1>" #'org-save-all-org-buffers)
(keymap-set org-agenda-mode-map "M-p" #'org-agenda-previous-date-line)
(keymap-set org-agenda-mode-map "M-n" #'org-agenda-next-date-line)
(keymap-set org-agenda-mode-map "." #'cc/org-agenda-goto-now)

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
