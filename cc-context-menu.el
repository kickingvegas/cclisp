;; context-menu addons

(require 'cc-context-menu-macros)
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'cc-region-operations-menu)
(require 'cc-insert-org-plot)
(require 'cc-find-menu)

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)

    (cc/add-context-menu-item menu
                              status-report
                              "Journal"
                              "Go to current day journal")

    (cc/context-menu-item-separator menu buffer-navigation-separator)
    
    (cc/add-context-menu-item menu
                              list-buffers
                              "List All Buffers"
                              "List all buffers")

    (when buffer-file-name
      (cond ((use-region-p)
             (cc/context-menu-item-separator menu narrow-separator)
             (cc/add-context-menu-item menu
                                       narrow-to-region
                                       (cc/context-menu-label "Narrow Region")
                                       "Restrict editing in this buffer to the current region"))
            
            ((and (not (buffer-narrowed-p)) (derived-mode-p 'org-mode))
             (cc/context-menu-item-separator menu narrow-separator)
             (cc/add-context-menu-item menu
                                       org-narrow-to-subtree
                                       "Narrow to Subtree"
                                       "Restrict editing in this buffer to the current subtree"))
            
            ((and (not (buffer-narrowed-p)) (derived-mode-p 'markdown-mode))
             (cc/context-menu-item-separator menu narrow-separator)
             (cc/add-context-menu-item menu
                                       markdown-narrow-to-subtree
                                       "Narrow to Subtree"
                                       "Restrict editing in this buffer to the current subtree")))
      
      (when (buffer-narrowed-p)
        (cc/context-menu-item-separator menu widen-separator)
        (cc/add-context-menu-item menu
                                  widen
                                  "Widen buffer"
                                  "Remove narrowing restrictions from current buffer")))
    
    (cc/context-menu-item-separator menu capture-flow-separator)
    (cc/add-context-menu-item menu
                              org-capture
                              "New Workflow…"
                              "Create new task or workflow via org-capture")

    (cc/context-menu-item-separator menu open-in-separator)

    (when buffer-file-name
      (cc/add-context-menu-item menu
                                reveal-in-folder-this-buffer
                                "Open in Finder"
                                "Open file (buffer) in Finder")

      (when (not (derived-mode-p 'dired-mode))
        (cc/add-context-menu-item menu
                                  dired-jump-other-window
                                  "Open in Dired"
                                  "Open file in Dired")))
    
    (when (derived-mode-p 'dired-mode)
      (cc/add-context-menu-item menu
                                cc/dired-duplicate-file
                                (concat "Duplicate"
                                        " “"
                                        (file-name-base (dired-get-filename))
                                        "."
                                        (file-name-extension (dired-get-filename))
                                        "”")
                                "Duplicate selected item"))

    (when (use-region-p)
      (cc/context-menu-item-separator menu dictionary-operations-separator)
      (cc/add-context-menu-item menu
                                osx-dictionary-search-word-at-point
                                (cc/context-menu-last-word-in-region "Look Up")
                                "Look up selected region in macOS dictionary"))

    (cc/context-menu-item-separator menu find-operations-separator)
    (if (use-region-p)
        (cc/add-context-menu-item menu
                                  occur-word-at-mouse
                                  (cc/context-menu-last-word-in-region "Find word in buffer (occur)")
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
    
    (when (use-region-p)
      (cc/context-menu-item-separator menu transform-text-separator)
      (cc/add-context-menu-submenu menu
                                   cc/transform-text-menu
                                   "Transform")

      (cond
       ((derived-mode-p 'prog-mode)
        (cc/add-context-menu-item menu
                                  comment-region
                                  "Toggle Comment"
                                  "Toggle comment on selected region"))

       ((derived-mode-p 'org-mode)
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
                                  "Copy as RTF to clipboard"))

       ((derived-mode-p 'markdown-mode)
        (cc/add-context-menu-submenu menu
                                     cc/markdown-emphasize-menu
                                     "Style"))))

    (cond
     ((derived-mode-p 'org-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (cc/add-context-menu-item menu
                                visible-mode
                                "Toggle Reveal Markup"
                                "Toggle making all invisible text \
temporarily visible (Visible mode)")
      (cc/add-context-menu-item-enable menu
                                       org-insert-last-stored-link
                                       "Paste Last Org Link"
                                       "Insert the last link stored in org-stored-links"
                                       (cc/org-stored-links-p)))

     ((derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu markdown-mode-operations-separator)
      (cc/add-context-menu-item menu
                                markdown-toggle-markup-hiding
                                "Toggle Reveal Markup"
                                "Toggle the display or hiding of markup")))
     
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

    (when (use-region-p)
      (cc/context-menu-item-separator menu region-operations-separator)
      (cc/add-context-menu-submenu menu
                                   cc/region-operations-menu
                                   "Operate on Region"))

    (cc/context-menu-item-separator menu world-clock-separator)
    (cc/add-context-menu-item menu
                              calendar
                              "Calendar"
                              "Display a three-month Gregorian calendar")
    (cc/add-context-menu-item menu
                              world-clock
                              "World Clock"
                              "Display times from around the world")

    (cc/context-menu-item-separator menu count-words-separator)

    (when (derived-mode-p 'text-mode)
      (if (use-region-p)
          (cc/add-context-menu-item menu
                                    count-words
                                    "Count Words in Region"
                                    "Count words in region")
    
        (cc/add-context-menu-item menu
                                  count-words
                                  "Count Words in Buffer"
                                  "Count words in buffer")))

    menu))

(defun cc/kill-org-table-reference (e)
  (interactive "e")
  (kill-new (format "@%d$%d"
                    (org-table-current-dline)
                    (org-table-current-column))))

(add-hook 'context-menu-functions #'cc/context-menu-addons)

(provide 'cc-context-menu)
