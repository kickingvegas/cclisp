;; context-menu addons

(load "cc-transform-text-menu")
(load "cc-style-text-menu")
(load "cc-insert-org-plot")

(defun cc/context-menu-label (prefix)
  (let ((start (region-beginning))
        (end (region-end))
        (buf "")
        (max 25)
        (size (abs (- (region-end) (region-beginning)))))
    (if (> size max)
        (setq buf (concat prefix " “"(buffer-substring start (+ max start)) "…"))
      (setq buf (concat prefix " “" (buffer-substring start end) "”")))
    buf))

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)

    (define-key-after menu [next-buffer]
      '(menu-item "Next Buffer" next-buffer
                  :help "Go to next buffer"))

    (define-key-after menu [previous-buffer]
      '(menu-item "Previous Buffer" previous-buffer
                  :help "Go to previous buffer"))

    (define-key-after menu [list-all-buffers]
      '(menu-item "List All Buffers" list-buffers
                  :help "List all buffers"))
    
    (define-key-after menu [buffer-navigation-separator]
      '(menu-item "--single-line"))
        
    (define-key-after menu [capture-flow]
      '(menu-item "Capture Flow" org-capture
                  :help "Use Org Capture to make a flow"))

    (define-key-after menu [capture-flow-separator]
      '(menu-item "--single-line"))
        
    (define-key-after menu [open-in-finder]
      '(menu-item "Open in Finder" reveal-in-folder-this-buffer
                  :help "Open file (buffer) in Finder"))

    (when (region-active-p)
      (define-key-after menu [osx-dictionary-lookup]
        '(menu-item (cc/context-menu-label "Look Up") osx-dictionary-search-word-at-point
                    :help "Look up in dictionary"))

      (define-key-after menu [occur-word-at-mouse]
        '(menu-item (cc/context-menu-label "Occur") occur-word-at-mouse
                    :help "Occur")))
    
    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (define-key-after menu [vc-separator]
        '(menu-item "--single-line"))
      
      (define-key-after menu [magit-status]
        '(menu-item "Magit Status" magit-status
                    :help "Magit Status"))
      (define-key-after menu [ediff-revision]
        '(menu-item "Ediff revision…" cc/ediff-revision
                    :help "Ediff this file with revision"))

      (define-key-after menu [magit-history]
        '(menu-item "Git history" magit-log-buffer-file
                    :help "History")))
    
    (when (region-active-p)
      (define-key-after menu [transform-text-separator]
        '(menu-item "--single-line"))
      (define-key-after menu [tranform-text]
        (list 'menu-item "Transform" cc/transform-text-menu)))


    (when (and (derived-mode-p 'markdown-mode) (region-active-p))
      (define-key-after menu [markdown-emphasize]
        (list 'menu-item "Style" cc/markdown-emphasize-menu)))
    
    (when (and (derived-mode-p 'org-mode) (region-active-p))
      (define-key-after menu [org-emphasize]
        (list 'menu-item "Style" cc/org-emphasize-menu))

      (define-key-after menu [org-export-to-slack]
        '(menu-item "Copy as Slack" org-slack-export-to-clipboard-as-slack
                    :help "Copy as Slack to clipboard"))
      
      (define-key-after menu [copy-as-rtf]
        '(menu-item "Copy as RTF" dm/copy-as-rtf
                    :help "Copy as RTF to clipboard")))

    (when (derived-mode-p 'org-mode)
      (define-key-after menu [org-visible-mode-separator]
        '(menu-item "--single-line"))
      
      (define-key-after menu [org-visible-mode]
        '(menu-item "Toggle reveal markup" visible-mode
                    :help "Toggle reveal markup"))

      (define-key-after menu [org-insert-last-stored-link]
        '(menu-item "Paste last Org link" org-insert-last-stored-link
                    :help "Paste last stored Org link")))
    
    (when (derived-mode-p 'markdown-mode)
      (define-key-after menu [markdown-visible-mode-separator]
        '(menu-item "--single-line"))
      
      (define-key-after menu [markdown-visible-mode]
        '(menu-item "Toggle reveal markup" markdown-toggle-markup-hiding
                    :help "Toggle reveal markup")))
    
    (when (org-at-table-p)
      (define-key-after menu [org-table-separator]
        '(menu-item "--single-line"))
      (define-key-after menu [org-table-field-info]
        '(menu-item (format "@%d$%d"
                            (org-table-current-dline)
                            (org-table-current-column))
                    cc/kill-org-table-reference
                    :help "Table field/cell information"))
      (define-key-after menu [org-table-insert-plot]
        (list 'menu-item "Insert Plot" cc/insert-org-plot-menu))
      
      (define-key-after menu [org-plot-gnuplot]
        '(menu-item "Run gnuplot" org-plot/gnuplot
                    :help "Run gnuplot")))
    
    (when (region-active-p)
      (define-key-after menu [search-separator]
        '(menu-item "--single-line"))
      
      (define-key-after menu [google-search]
        '(menu-item (cc/context-menu-label "Search with Google") google-this-noconfirm
                    :help "Search Google with region"))

      (define-key-after menu [google-translate]
        '(menu-item (cc/context-menu-label "Translate") google-translate-smooth-translate
                    :help "Translate using Google Translate"))
      
      (define-key-after menu [webpaste-paste-region]
        '(menu-item (cc/context-menu-label "Upload to Webpaste") webpaste-paste-region
                    :help "Upload region to Webpaste"))))
  
  (define-key-after menu [world-clock-separator]
    '(menu-item "--single-line"))

  (define-key-after menu [calendar]
    '(menu-item "Calendar" calendar
                :help "Display calendar"))
  (define-key-after menu [world-clock]
    '(menu-item "World Clock" world-clock
                :help "Display world clock"))

  (when (region-active-p)
    (define-key-after menu [speech-separator]
      '(menu-item "--single-line"))
    
    (define-key-after menu [google-translate]
      '(menu-item "Start Speaking" cc/say-region
                  :help "Start speaking region")))
    
  menu)


(defun cc/kill-org-table-reference (e)
  (interactive "e")
  (kill-new (format "@%d$%d"
                    (org-table-current-dline)
                    (org-table-current-column))))

(add-hook 'context-menu-functions #'cc/context-menu-addons)
