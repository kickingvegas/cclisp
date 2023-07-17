;; context-menu addons

(require 'cc-context-menu-macros)
(require 'cc-transform-text-menu)
(require 'cc-style-text-menu)
(require 'cc-region-operations-menu)
(require 'cc-insert-org-plot)
(require 'cc-find-menu)

(easy-menu-define cc/transpose-menu nil
  "Keymap for Transpose submenu"
  '("Transpose"
    ["Characters" transpose-chars
     :help "Interchange characters around point, moving forward one character."]
    
    ["Words" transpose-words
     :help "Interchange words around point, leaving point at end of them."]
     
    ["Lines" transpose-lines
     :help "Exchange current line and previous line, leaving point after both."]
  
    ["Sentences" transpose-sentences
     :help "Interchange the current sentence with the next one."]
     
    ["Paragraphs" transpose-paragraphs
     :help "Interchange the current paragraph with the next one."]

    ["Regions" transpose-regions
     :help "region STARTR1 to ENDR1 with STARTR2 to ENDR2."]))

(defun cc/context-menu-addons (menu click)
  "CC context menu additions"
  (save-excursion
    (mouse-set-point click)

    (easy-menu-add-item menu nil ["Journal"
                                  status-report
                                  :help "Go to current day journal"])

    (cc/context-menu-item-separator menu buffer-navigation-separator)
        
    (easy-menu-add-item menu nil ["List All Buffers"
                                  list-buffers
                                  :help "List all buffers"])

    (when buffer-file-name
      (cond ((use-region-p)
             (cc/context-menu-item-separator menu narrow-separator)
             (easy-menu-add-item menu nil
                                 ["Narrow Region" narrow-to-region
                                  :label (cc/context-menu-label "Narrow Region")
                                  :help "Restrict editing in this buffer \
to the current region"]))
            
            
            ((and (not (buffer-narrowed-p)) (derived-mode-p 'org-mode))
             (cc/context-menu-item-separator menu narrow-separator)             
             (easy-menu-add-item menu nil
                                 ["Narrow to subtree" org-narrow-to-subtree
                                  :help "Restrict editing in this buffer \
to the current subtree"]))
             
            
            ((and (not (buffer-narrowed-p)) (derived-mode-p 'markdown-mode))
             (cc/context-menu-item-separator menu narrow-separator)             
             (easy-menu-add-item menu nil
                                 ["Narrow to subtree" markdown-narrow-to-subtree
                                  :help "Restrict editing in this buffer \
to the current subtree"])))
      
      (when (buffer-narrowed-p)
        (cc/context-menu-item-separator menu widen-separator)
        (easy-menu-add-item menu nil
                            ["Widen buffer" widen
                             :help "Remove narrowing restrictions \
from current buffer"])))

    (easy-menu-add-item menu nil "--")      
    
    (cc/context-menu-item-separator menu capture-flow-separator)
    (easy-menu-add-item menu nil
                        ["New Workflow…"
                         org-capture
                         :help "Create new task or workflow via org-capture"])

    (cc/context-menu-item-separator menu open-in-separator)

    (when buffer-file-name
      (easy-menu-add-item menu nil
                          ["Open in Finder"
                           reveal-in-folder-this-buffer
                           :help "Open file (buffer) in Finder"])

      (when (not (derived-mode-p 'dired-mode))
        (easy-menu-add-item menu nil
                            ["Open in Dired"
                             dired-jump-other-window
                             :help "Open file in Dired"])))
    
    (when (derived-mode-p 'dired-mode)
      (easy-menu-add-item menu nil
                          ["Duplicate"
                           cc/dired-duplicate-file
                           :label (concat "Duplicate"
                                          " “"
                                          (file-name-base (dired-get-filename))
                                          "."
                                          (file-name-extension (dired-get-filename))
                                          "”")
                           :help "Duplicate selected item"]))

    (when (use-region-p)
      (cc/context-menu-item-separator menu dictionary-operations-separator)
      (easy-menu-add-item menu nil ["Look Up" 
                                    osx-dictionary-search-word-at-point
                                    :label (cc/context-menu-last-word-in-region "Look Up")                                    
                                    :help "Look up selected region in macOS dictionary"]))

    (cc/context-menu-item-separator menu find-operations-separator)
    (if (use-region-p)
        (easy-menu-add-item menu nil
                            ["Find word in buffer (occur)"
                             occur-word-at-mouse
                             :label (cc/context-menu-last-word-in-region
                                     "Find word in buffer (occur)")
                             :help "Show all lines in the current buffer containing \
a match for selected word"])
      (easy-menu-add-item menu nil
                          ["Find in buffer (occur)"
                           occur
                           :help "Show all lines in the current buffer \
containing a match for regex"]))
    
    (easy-menu-add-item menu nil cc/find-menu)
            
    (when (and (bound-and-true-p buffer-file-name)
               (vc-registered (buffer-file-name)))
      (cc/context-menu-item-separator menu vc-separator)
      (easy-menu-add-item menu nil
                          ["Magit Status"
                           magit-status
                           :help "Show the status of the current Git repository \
in a buffer"])
      (easy-menu-add-item menu nil ["Ediff revision…"
                                    cc/ediff-revision-from-menu
                                    :help "Ediff this file with revision"])
      (easy-menu-add-item menu nil ["Git History"
                                    magit-log-buffer-file
                                    :help "Show log for the blob or file visited in \
the current buffer"]))
    
    (when (use-region-p)
      (cc/context-menu-item-separator menu transform-text-separator)
      (easy-menu-add-item menu nil cc/transform-text-menu)

      (cond
       ((derived-mode-p 'prog-mode)
        (easy-menu-add-item menu nil
                            ["Toggle Comment"
                             comment-region
                             :help "Toggle comment on selected region"]))

       ((derived-mode-p 'org-mode)
        (easy-menu-add-item menu nil cc/emphasize-menu)
        (easy-menu-add-item menu nil ["Copy as Slack"
                                      org-slack-export-to-clipboard-as-slack
                                      :help "Copy as Slack to clipboard"])
        (easy-menu-add-item menu nil ["Copy as Slack"
                                      org-slack-export-to-clipboard-as-slack
                                      :help "Copy as Slack to clipboard"])
        (easy-menu-add-item menu nil ["Copy as RTF"
                                      dm/copy-as-rtf
                                      :help "Copy as RTF to clipboard"]))

       ((derived-mode-p 'markdown-mode)
        (easy-menu-add-item menu nil cc/emphasize-menu))))

    (cond
     ((derived-mode-p 'org-mode)
      (cc/context-menu-item-separator menu org-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Reveal Markup"
                           visible-mode
                           :help "Toggle making all invisible text \
temporarily visible (Visible mode)"])

      (easy-menu-add-item menu nil
                          ["Paste Last Org Link"
                           org-insert-last-stored-link
                           :enable (cc/org-stored-links-p)
                           :help "Insert the last link stored in org-stored-links"]))

     ((derived-mode-p 'markdown-mode)
      (cc/context-menu-item-separator menu markdown-mode-operations-separator)
      (easy-menu-add-item menu nil
                          ["Toggle Reveal Markup"
                           markdown-toggle-markup-hiding
                           :help "Toggle the display or hiding of markup"])))
     
    (when (org-at-table-p)
      (cc/context-menu-item-separator menu org-table-separator)
      (easy-menu-add-item menu nil
                          ["Table Cell Info"
                           cc/kill-org-table-reference
                           :label (format "@%d$%d"
                                          (org-table-current-dline)
                                          (org-table-current-column))
                           :help "Table field/cell information"])
      (easy-menu-add-item menu nil cc/insert-org-plot-menu)
      (easy-menu-add-item menu nil ["Run gnuplot"
                                    org-plot/gnuplot
                                    :help "Plot table using gnuplot"]))

    (when (use-region-p)
      (cc/context-menu-item-separator menu region-operations-separator)
      (easy-menu-add-item menu nil cc/region-operations-menu))

    (cc/context-menu-item-separator menu world-clock-separator)
    (easy-menu-add-item menu nil
                        ["Calendar"
                         calendar
                         :help "Display a three-month Gregorian calendar"])
    (easy-menu-add-item menu nil
                        ["World Clock"
                         world-clock
                         :help "Display times from around the world"])

    (cc/context-menu-item-separator menu count-words-separator)

    (when (derived-mode-p 'text-mode)
      (if (use-region-p)
          (easy-menu-add-item menu nil ["Count Words in Region"
                                        count-words
                                        :help "Count words in region"])
    
        (easy-menu-add-item menu nil ["Count Words in Buffer"
                                      count-words
                                      :help "Count words in buffer"])))

    
    (easy-menu-add-item menu nil cc/transpose-menu)
    (easy-menu-add-item menu nil ["Join Line"
                                  join-line
                                  :help "Join this line to previous and fix up \
whitespace at join"])
    
    menu))

(defun cc/kill-org-table-reference (e)
  (interactive "e")
  (kill-new (format "@%d$%d"
                    (org-table-current-dline)
                    (org-table-current-column))))

(add-hook 'context-menu-functions #'cc/context-menu-addons)

(provide 'cc-context-menu)
