;;

(require 'cc-context-menu-macros)
(require 'markdown-mode)

;; (defun cc/org-emphasize-reset ()
;;   ;; this won't work when org-hide-emphasis-markers is turned on.
;;   (interactive)
;;   (org-emphasize ?\s))

(defun cc/emphasize-bold ()
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?*))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-bold))
        (t nil)))

(defun cc/emphasize-italic ()
  (interactive)  
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?/))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-italic))
        (t nil)))

(defun cc/emphasize-code ()
  (interactive)  
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?~))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-code))
        (t nil)))

(defun cc/emphasize-underline ()
  (interactive)  
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?_))
        (t nil)))

(defun cc/emphasize-verbatim ()
  (interactive)
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?=))
        (t nil)))

(defun cc/emphasize-strike-through ()
  (interactive)  
  (cond ((derived-mode-p 'org-mode)
         (org-emphasize ?+))
        ((derived-mode-p 'markdown-mode)
         (markdown-insert-strike-through))
        (t nil)))

(easy-menu-define cc/emphasize-menu nil
  "Keymap for Emphasize Menu"
  '("Style"
    :visible (region-active-p)
    ["Bold" cc/emphasize-bold
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))
     :help "Bold selected region"]
    ["Italic" cc/emphasize-italic
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))     
     :help "Italic selected region"]
    ["Code" cc/emphasize-code
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))     
     :help "Code selected region"]
    ["Underline" cc/emphasize-underline
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)     
     :help "Underline selected region"]
    ["Verbatim" cc/emphasize-verbatim
     :enable (region-active-p)
     :visible (derived-mode-p 'org-mode)
     :help "Verbatim selected region"]
    ["Strike Through" cc/emphasize-strike-through
     :enable (region-active-p)
     :visible (or (derived-mode-p 'org-mode) (derived-mode-p 'markdown-mode))     
     :help "Strike-through selected region"]))

(provide 'cc-style-text-menu)
