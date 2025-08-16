;;; cc-org-mode.el --- Org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Charles Choi

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
(require 'org-mouse)
(require 'org-superstar)
(require 'face-remap)
(require 'org-ql)
(require 'cclisp)
(require 'cc-save-hooks)
(require 'company)
(require 'hl-line)
(require 'prog-mode)
(require 'cc-org-smart-quotes)
(require 'imenu)
(require 'casual-agenda)
(require 'cc-style-text-menu)
(require 'org-protocol)
;;(require 'casual-calendar)

(if (eq system-type 'darwin)
    (require 'ob-swiftui))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; default for dev5
(defvar cc/org-daily-header-template "CC Notes - %a %b %d %Y"
  "Custom daily Org header template.")

(defun cc/org-checkbox-in-progress ()
  "If point is on an Org list item, set it to be a checkbox in-progress."
  (interactive)
  (if (org-at-item-checkbox-p)
      (org-ctrl-c-ctrl-c '(16))
    (org-ctrl-c-ctrl-c '(4))))

(defun cc/org-toggle-list-is-checkbox ()
  "If point is on an Org list item, toggle if the list item is also a checkbox.
Note that this function does not toggle the actual value of a checkbox,
which is done with `org-ctrl-c-ctrl-c'."
  (interactive)
  (org-ctrl-c-ctrl-c '(4)))

(setopt org-default-notes-file "~/org/notes.org")

(setopt org-todo-keywords
           '((sequence "TODO(t)" "IN_PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
             (sequence "|" "CANCELED(c)")))

;; (setq org-todo-keyword-faces
;;       '(("TODO" . "red")
;;         ("IN_PROGRESS" . "dark orange")
;;         ("WAITING" . "dark orange")
;;         ("DONE" . "sea green")
;;         ("CANCELED" . (:foreground "blue" :weight bold))))

(setopt org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

;;(setq org-log-done 'time)

(add-hook 'org-mode-hook #'org-superstar-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
;;(add-hook 'org-mode-hook #'cc/save-hook-delete-trailing-whitespace)
(add-hook 'org-mode-hook (lambda ()
                           (cc/reconfig-org-smart-quotes-lang "en")))

(setopt org-imenu-depth 7)
(add-hook 'org-mode-hook #'imenu-add-menubar-index)
(add-hook 'org-mode-hook (lambda () (setq-local imenu-auto-rescan t)))
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
                            ("[#A]" . ?🄰 )
                            ("[#B]" . ?🄱 )
                            ("[#C]" . ?🄲 )
                            ("#+NAME:" . ?📇 )
                            ("#+TBLFM:" . ?🧮 )
                            ("#+PLOT:" . ?📊 )
                            (":CREATED:" . ?𝛼 )
                            ("CLOCK:" . ?⌛ )
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
                             "verse"
                             "minipage"
                             "infobox"
                             "warningbox"
                             "blindtext")))
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
(keymap-set org-mode-map "C-/" #'cc/emphasize-dwim)
(keymap-set org-mode-map "C-_" #'cc/emphasize-dwim)
(keymap-set org-mode-map "s-e" #'cc/emphasize-dwim)
(keymap-set org-mode-map "s-b" #'cc/emphasize-bold)
(keymap-set org-mode-map "s-i" #'cc/emphasize-italic)
(keymap-set org-mode-map "s-c" #'cc/emphasize-code)
(keymap-set org-mode-map "s-u" #'cc/emphasize-underline)
(keymap-set org-mode-map "s-r" #'cc/emphasize-remove)
(keymap-set org-mode-map "s-s" #'cc/emphasize-strike-through)
(keymap-set org-mode-map "s-<tab>" #'completion-at-point)

(keymap-set org-mode-map "C-6" #'org-goto)
;; (keymap-set org-read-date-minibuffer-local-map "C-o" #'casual-calendar)

(add-hook 'org-agenda-finalize-hook 'hl-line-mode)
(add-hook 'org-agenda-finalize-hook
          (lambda ()
            (define-key org-agenda-mode-map
              [(double-mouse-1)] 'org-agenda-goto-mouse)))

(keymap-set org-agenda-mode-map "<f1>" #'org-save-all-org-buffers)
(keymap-set org-agenda-mode-map "M-p" #'org-agenda-previous-date-line)
(keymap-set org-agenda-mode-map "M-n" #'org-agenda-next-date-line)
(keymap-set org-agenda-mode-map "." #'casual-agenda-goto-now)

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
   (gnuplot . t)
   (swift . t)))

(when (fboundp 'ob-swiftui-setup)
  (ob-swiftui-setup))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; (define-key org-mode-map
;;   [menu-bar table table-field-info]
;;   '("Table Field Info" . org-table-field-info))


(require 'ox-publish)

(setopt org-publish-project-alist
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
  (if (string= (system-name) "dev7")
      (org-capture nil "J")
    (org-capture nil "j")))

(defalias 'cc/insert-org-keyword
  (kmacro "C-a # + M-x c o m p l e t e - s y m b o l <return>"))

(require 'cc-org-capture)

(transient-define-prefix cc/org-mode-tmenu ()
  ["Org"
   ["State"
    ("t" "TODO" org-todo)
    ("I" "Clock In" org-clock-in
     :if-not org-clocking-p)
    ("O" "Clock Out" org-clock-out
     :if org-clocking-p)]

   ["Timestamp"
    ("." "Add" org-timestamp)
    ("i" "Inactive" org-timestamp-inactive)]

   ["Link"
    ("l" "Insert" org-insert-link)
    ("L" "Last" org-insert-last-stored-link)]

   ["Schedule"
    ("s" "Schedule…" org-schedule)
    ("d" "Deadline…" org-deadline)]

   ["Edit"
    ("b" "Add Block" org-insert-structure-template)
    ("c" "Capture" org-capture)
    ("C" "Clone" org-clone-subtree-with-time-shift)]

   ["Annotate"
    ("p" "Property" org-set-property)
    (":" "Tags" org-set-tags-command)
    ("n" "Note" org-add-note)]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(keymap-set org-mode-map "M-m" #'cc/org-mode-tmenu)

(provide 'cc-org-mode)
;;; cc-org-mode.el ends here
