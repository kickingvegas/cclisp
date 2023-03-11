;; org-mode

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; default for dev5
(setq cc/org-daily-header-template "CC Notes - %a %b %d %Y")

;(when (string-equal (system-name) "bingsu.local")
                                        ;  (setq cc/org-daily-header-template "Charles Choi Notes - %a %b %d %Y"))

(defun cc/refresh-header-timestamps ()
  (interactive)

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
          ("c" "Captee Capture" entry (file+headline ,org-default-notes-file ,cc-org-daily-header)
	   "* %:description\n%:annotation\n%i\n%?" :empty-lines 1)
          ("i" "Issue Capture" entry (file+headline ,org-default-notes-file ,cc-org-daily-header)
	   "* TODO [#B] %:description\n%:annotation\n%i\n%?" :empty-lines 1)
          )))

(cc/refresh-header-timestamps)

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

(add-hook 'org-mode-hook 'org-superstar-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'company-mode)

(add-hook 'org-mode-hook (lambda ()
			   (define-key org-mode-map (kbd "<f8>") 'datestamp)
			   (define-key org-mode-map (kbd "<f9>") 'cc-todo-item)
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
                           (add-to-list (make-local-variable 'company-backends)
                                        'company-org-block)))

(add-hook 'org-agenda-finalize-hook 'hl-line-mode)

;; (setq org-agenda-finalize-hook
;;       '((lambda ()
;; 	  (hl-line-mode 1))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (emacs-lisp . t)
   (python . t)
   (shell . t)
   (sql . t)
   (restclient . t)
   (plantuml . t)
   (swift . t)))

(when (string-equal (system-name) "bingsu.local")
  (require 'ob-swiftui)
  (ob-swiftui-setup))

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; (define-key org-mode-map
;;   [menu-bar table table-field-info]
;;   '("Table Field Info" . org-table-field-info))

(eval-after-load "org"
  '(require 'ox-gfm nil t))
