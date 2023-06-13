;; context-menu addons

(defmacro cc/context-menu-item-separator (menu key)
  "Add single line separator to MENU with KEY."
  `(define-key-after ,menu [,key]
     '(menu-item "--single-line")))

(defmacro cc/add-context-menu-item (menu command label help)
  "Add COMMAND to MENU annotated with LABEL and HELP."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-first-context-menu-item (menu command label help)
  "Add first COMMAND to MENU annotated with LABEL and HELP."
  `(define-key ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-submenu (menu submenu label)
  "Add SUBMENU to MENU annotated with LABEL.\n\n\
SUBMENU is a keymap. "
  `(define-key-after ,menu [,submenu]
     (list 'menu-item ,label ,submenu)))

(defun cc/context-menu-label (prefix)
  (let ((start (region-beginning))
        (end (region-end))
        (buf "")
        (max 25)
        (size (abs (- (region-end) (region-beginning)))))
    (if (> size max)
        (setq buf (concat prefix " “"(buffer-substring start (+ max start)) "…”"))
      (setq buf (concat prefix " “" (buffer-substring start end) "”")))
    buf))

(load "cc-transform-text-menu")
(load "cc-style-text-menu")
(load "cc-insert-org-plot")
(load "cc-find-menu")

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)

    (cc/add-context-menu-item menu
                              status-report
                              "Status Report"
                              "Go to current day journal")

    (cc/context-menu-item-separator menu buffer-navigation-separator)
    
    (cc/add-context-menu-item menu
                              previous-buffer                          
                              "Previous Buffer"
                              "Go to previous buffer")

    (cc/add-context-menu-item menu
                              next-buffer
                              "Next Buffer"
                              "Go to next buffer")
    
    (cc/add-context-menu-item menu
                              list-buffers
                              "List All Buffers"
                              "List all buffers")

    (when (region-active-p)
      (cc/context-menu-item-separator menu narrow-to-region-separator)
      (cc/add-context-menu-item menu
                                narrow-to-region
                                (cc/context-menu-label "Narrow region")
                                "Restrict editing in this buffer to the current region"))
    
    (when (buffer-narrowed-p)
      (cc/context-menu-item-separator menu widen-separator)
      (cc/add-context-menu-item menu
                                widen
                                "Widen buffer"
                                "Remove narrowing restrictions from current buffer"))
    
    (cc/context-menu-item-separator menu capture-flow-separator)
    (cc/add-context-menu-item menu
                              org-capture
                              "New Workflow…"
                              "Create new task or workflow via org-capture")

    (cc/context-menu-item-separator menu open-in-separator)
    (cc/add-context-menu-item menu
                              reveal-in-folder-this-buffer
                              "Open in Finder"
                              "Open file (buffer) in Finder")

    (when (not (derived-mode-p 'dired-mode))
      (cc/add-context-menu-item menu
                                dired-jump-other-window
                                "Open in Dired"
                                "Open file in Dired"))

    (when (region-active-p)
      (cc/context-menu-item-separator menu dictionary-operations-separator)
      (cc/add-context-menu-item menu
                                osx-dictionary-search-word-at-point
                                (cc/context-menu-label "Look Up")
                                "Look up selected region in macOS dictionary"))

    (cc/context-menu-item-separator menu find-operations-separator)
    (if (region-active-p)
        (cc/add-context-menu-item menu
                                  occur-word-at-mouse
                                  (cc/context-menu-label "Find word in buffer (occur)")
                                  "Show all lines in the current buffer containing \
a match for selected word")
      (cc/add-context-menu-item menu
                                occur
                                "Find in buffer (occur)"
                                "Show all lines in the current buffer \
containing a match for regex"))
    
    (cc/add-context-menu-submenu menu
                                 cc/find-menu
                                 "Find and/or replace")
            
    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (cc/context-menu-item-separator menu vc-separator)
      (cc/add-context-menu-item menu
                                magit-status
                                "Magit Status"
                                "Show the status of the current Git repository \
in a buffer")
      (cc/add-context-menu-item menu
                                cc/ediff-revision
                                "Ediff revision…"
                                "Ediff this file with revision")
      (cc/add-context-menu-item menu
                                magit-log-buffer-file
                                "Git History"
                                "Show log for the blob or file visited in \
the current buffer"))
    
    (when (region-active-p)
      (cc/context-menu-item-separator menu transform-text-separator)
      (cc/add-context-menu-submenu menu
                                   cc/transform-text-menu
                                   "Transform")

      (when (derived-mode-p 'prog-mode)
        (cc/add-context-menu-item menu
                                  comment-region
                                  "Toggle Comment"
                                  "Toggle comment on selected region"))

      (when (derived-mode-p 'markdown-mode)
        (cc/add-context-menu-submenu menu
                                     cc/markdown-emphasize-menu
                                     "Style"))

      (when (derived-mode-p 'org-mode)
        (cc/add-context-menu-submenu menu
                                     cc/org-emphasize-menu
                                     "Style")

        (cc/add-context-menu-item menu
                                  org-slack-export-to-clipboard-as-slack
                                  "Copy as Slack"
                                  "Copy as Slack to clipboard")
        (cc/add-context-menu-item menu
                                  dm/copy-as-rtf
                                  "Copy as RTF"
                                  "Copy as RTF to clipboard")))

    (when (derived-mode-p 'org-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (cc/add-context-menu-item menu
                                visible-mode
                                "Toggle Reveal Markup"
                                "Toggle making all invisible text \
temporarily visible (Visible mode)")
      (cc/add-context-menu-item menu
                                org-insert-last-stored-link
                                "Paste Last Org Link"
                                "Insert the last link stored in org-stored-links"))
    
    (when (derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (cc/add-context-menu-item menu
                                markdown-toggle-markup-hiding
                                "Toggle Reveal Markup"
                                "Toggle the display or hiding of markup"))
    
    (when (org-at-table-p)
      (cc/context-menu-item-separator menu org-table-separator)
      (cc/add-context-menu-item menu
                                cc/kill-org-table-reference
                                (format "@%d$%d"
                                        (org-table-current-dline)
                                        (org-table-current-column))
                                "Table field/cell information")
      (cc/add-context-menu-submenu menu
                                   cc/insert-org-plot-menu
                                   "Insert Plot")
      (cc/add-context-menu-item menu
                                org-plot/gnuplot
                                "Run gnuplot"
                                "Plot table using gnuplot"))
    
    (when (region-active-p)
      (cc/context-menu-item-separator menu external-operations-separator)
      (when (cc/phone-number-p)
        (cc/add-context-menu-item menu
                                  cc/make-phone-call
                                  (cc/context-menu-label "Call")
                                  "Make phone call"))
      (cc/add-context-menu-item menu
                                google-this-noconfirm
                                (cc/context-menu-label "Search with Google")
                                "Search Google with selected region")
      (cc/add-context-menu-item menu
                                google-translate-smooth-translate
                                (concat (cc/context-menu-label "Translate") "…")
                                "Translate selected region with Google Translate")
      (cc/add-context-menu-item menu
                                webpaste-paste-region
                                (cc/context-menu-label "Upload to Webpaste")
                                "Upload selected region to paste service leaving \
link in the clipboard"))

    )

  (cc/context-menu-item-separator menu world-clock-separator)
  (cc/add-context-menu-item menu
                            calendar
                            "Calendar"
                            "Display a three-month Gregorian calendar")
  (cc/add-context-menu-item menu
                            world-clock
                            "World Clock"
                            "Display times from around the world")
  
  (when (region-active-p)
    (cc/context-menu-item-separator menu speech-separator)
    (cc/add-context-menu-item menu
                              cc/say-region
                              "Start Speaking"
                              "Start speaking selected region"))
  
  menu)

(defun cc/kill-org-table-reference (e)
  (interactive "e")
  (kill-new (format "@%d$%d"
                    (org-table-current-dline)
                    (org-table-current-column))))

(add-hook 'context-menu-functions #'cc/context-menu-addons)
