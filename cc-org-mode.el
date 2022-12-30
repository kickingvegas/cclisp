;; Org Mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; default for dev5
(setq cc/org-daily-header-template "CC Notes - %a %b %d %Y")

(when (string-equal (system-name) "bingsu.local")
  (setq cc/org-daily-header-template "Charles Choi Notes - %a %b %d %Y"))

(setf cc-org-daily-header (format-time-string cc/org-daily-header-template))
(setf org-default-notes-file (format-time-string "~/org/%Y_%m_%d.org"))

(setq org-capture-templates
      `(
        ("b" "BeOrg TODO" entry (file "~/org/refile-beorg.org")
         "* TODO [#B] %^{description}\nSCHEDULED: %^T\n%?" :empty-lines 1)
        ("t" "TODO" entry (file+headline ,org-default-notes-file ,cc-org-daily-header)
	 "* TODO [#B] %^{description}\nSCHEDULED: %^T\n%?" :empty-lines 1)
        ("j" "Journal" entry (file+headline ,org-default-notes-file ,cc-org-daily-header)
	 "%(datestamp2)\n%?" :empty-lines 1)
        ))

(setq org-todo-keywords
           '((sequence "TODO(t)" "IN_PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")
             (sequence "|" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . "red")
	("IN_PROGRESS" . "dark orange")
	("WAITING" . "dark orange")
	("DONE" . "sea green")
        ("CANCELED" . (:foreground "blue" :weight bold))
	))

(setq org-agenda-files
      (list "~/org/"))

(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3)))

(setq org-log-done 'time)

(defun cc-todo-item ()
  (interactive)
  (org-insert-heading)
  (org-schedule nil)
  (org-shiftup)
  (org-shiftright)
  (end-of-line))

(setq org-mode-hook
      '((lambda ()
	  (local-set-key (kbd "<f8>") 'datestamp)
	  (local-set-key (kbd "<f9>") 'cc-todo-item)
	  (local-set-key (kbd "M-<f6>") 'org-toggle-inline-images)
          (local-set-key (kbd "C-c t") 'cc/org-time-stamp-inactive)
          (local-set-key (kbd "<home>") 'org-beginning-of-line)
          (local-set-key (kbd "<end>") 'org-end-of-line)
          (local-set-key (kbd "A-<left>") 'org-backward-sentence)
          (local-set-key (kbd "A-<right>") 'org-forward-sentence)
          (local-set-key (kbd "A-M-<left>") 'org-backward-paragraph)
          (local-set-key (kbd "A-M-<right>") 'org-forward-paragraph)
          (local-set-key (kbd "C-<up>") 'org-previous-visible-heading)
          (local-set-key (kbd "C-<down>") 'org-next-visible-heading)
	  (org-superstar-mode 1)
	  (variable-pitch-mode 1)
	  (org-indent-mode 1)
	  (flyspell-mode 1)
          (company-mode 1)
          (context-menu-mode 1)
          (add-to-list (make-local-variable 'company-backends)
                       'company-org-block)
	  )))

(setq org-agenda-finalize-hook
      '((lambda ()
	  (hl-line-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   (sql . t)
   (restclient . t)
   (plantuml . t)))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

