;;; cc-org-mode.el --- Org configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Charles Choi

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
(require 'org-protocol)
(require 'casual-org)
(require 'anju-style-text)

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

;; (setq org-todo-keyword-faces
;;       '(("TODO" . "red")
;;         ("IN_PROGRESS" . "dark orange")
;;         ("WAITING" . "dark orange")
;;         ("DONE" . "sea green")
;;         ("CANCELED" . (:foreground "blue" :weight bold))))

;;(setq org-log-done 'time)

(add-hook 'org-mode-hook #'org-superstar-mode)
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'org-mode-hook #'org-indent-mode)
;;(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
;;(add-hook 'org-mode-hook #'cc/save-hook-delete-trailing-whitespace)

(add-hook 'org-mode-hook (lambda ()
                           (cc/reconfig-org-smart-quotes-lang "en")))

(add-hook
 'org-mode-hook
 (lambda ()
   (add-to-list (make-local-variable 'company-backends)
                'company-org-block)))

;; (add-hook 'org-mode-hook
;;           (lambda () (add-hook 'ediff-prepare-buffer-hook #'org-fold-show-all 0 t)))

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
     (let* ((cc-temp-list '(("#+results:" . ?∴)
                            (":properties:" . ?⚙)
                            (":end:" . ?🔚)
                            (":logbook:" . ?📓)
                            ("[#A]" . ?🄰)
                            ("[#B]" . ?🄱)
                            ("[#C]" . ?🄲)
                            ("#+name:" . ?📛)
                            ("#+tblfm:" . ?🧮)
                            ("#+plot:" . ?📊)
                            (":created:" . ?𝛼)
                            ("clock:" . ?⌛)
                            ("#+print_bibliography:" . ?📚)
                            ("[ ]" .  ?☐)
                            ("[x]" . ?☑)
                            ("[-]" . ?✈)
                            ("#+begin:" . ?⎧)
                            ("#+end:" . ?⎩)
                            ("#+caption:" . ?🪧))))
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
         (let* ((begin-key (concat "#+begin_" e))
                (begin-key-upper (upcase begin-key))
                (begin-sym (cc/--prettify-components ?⎧ e))
                (end-key (concat "#+end_" e))
                (end-key-upper (upcase end-key))
                (end-sym (cc/--prettify-components ?⎩ e)))

           (push (cons begin-key begin-sym) prettify-symbols-alist)
           (push (cons begin-key-upper begin-sym) prettify-symbols-alist)

           (push (cons end-key end-sym) prettify-symbols-alist)
           (push (cons end-key-upper end-sym) prettify-symbols-alist))))
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
(keymap-set org-mode-map "M-<f9>" #'casual-org-checkbox-in-progress)
(keymap-set org-mode-map "C-<f9>" #'casual-org-toggle-list-to-checkbox)
(keymap-set org-mode-map "<f9>" #'org-ctrl-c-ctrl-c)
(keymap-set org-mode-map "M-<f6>" #'casual-org-toggle-images)
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
(keymap-set org-mode-map "C-/" #'anju-style-dwim)
(keymap-set org-mode-map "C-_" #'anju-style-dwim)
(keymap-set org-mode-map "s-e" #'anju-style-dwim)
(keymap-set org-mode-map "s-b" #'anju-style-bold)
(keymap-set org-mode-map "s-i" #'anju-style-italic)
(keymap-set org-mode-map "s-c" #'anju-style-code)
(keymap-set org-mode-map "s-u" #'anju-style-underline)
(keymap-set org-mode-map "s-r" #'anju-style-remove)
(keymap-set org-mode-map "s-s" #'anju-style-strike-through)
(keymap-set org-mode-map "s-<tab>" #'completion-at-point)
(keymap-set org-mode-map "M-[" #'backward-sexp)
(keymap-set org-mode-map "M-]" #'forward-sexp)


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
   ;; (restclient . t)
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

;; (use-package ox-gfm
;;   :defer 3
;;   :after org)

(defun cc/journal-entry ()
  "Capture journal entry in Org."
  (interactive)
  (if (string= (system-name) "dev7")
      (org-capture nil "J")
    (org-capture nil "j")))

(require 'cc-org-capture)

(defun cc/disable-flycheck-in-org-src-block ()
  (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(add-hook 'org-src-mode-hook 'cc/disable-flycheck-in-org-src-block)


;; -------------------------------------------------------------------
;; Transients

(keymap-set org-mode-map "M-m" #'casual-org-tmenu)
(keymap-set org-table-fedit-map "M-m" #'casual-org-table-fedit-tmenu)
(keymap-set org-table-fedit-map "<f1>" #'casual-org-table-fedit-tmenu)


;; -------------------------------------------------------------------
(defun cc/--days-until (target &optional template)
  "Formatted string of days until TARGET.

- TARGET: date string that conforms to `parse-time-string'.
- TEMPLATE : format string that includes ‘%d’ specifier.

If TEMPLATE is nil, then a predefined format string will be
used."
  (let* ((template (if template
                       template
                     (concat "%d days until " target)))
         (days (org-time-stamp-to-now target))
         (msg (format template days)))
    msg))

(defun cc/days-until (arg)
  "Prompt user for date and show days until in the mini-buffer.

Use `org-read-date' to compute days until to display in the mini-buffer.

If prefix ARG is non-nil, then the computed result is stored in the
 `kill-ring'."
  (interactive "P")
  (let* ((target (org-read-date))
         (msg (cc/--days-until target)))
    (if arg
        (kill-new msg))
    (message msg)))

(defun cc/org-show-current-clock ()
  "Show current Org clocking task in mini-buffer."
  (interactive)
  (if (org-clocking-p)
      (let ((clocked-task (substring-no-properties (org-clock-get-clock-string))))
        (message "%s" clocked-task))
    (message "No clock task.")))

;; ox-gfm init is so broken. need to load it manually.
(eval-after-load "org"
  '(require 'ox-gfm nil t))

(provide 'cc-org-mode)
;;; cc-org-mode.el ends here
