;;

(defmacro cc/context-menu-item-separator (menu key)
  "Add single line separator to MENU with KEY."
  `(define-key-after ,menu [,key]
     '(menu-item "--single-line")))

(defmacro cc/add-context-menu-item (menu command label help)
  "Add COMMAND to MENU annotated with LABEL and property HELP."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help)))

(defmacro cc/add-context-menu-item-visible (menu command label help visible)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, VISIBLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :visible ,visible)))

(defmacro cc/add-context-menu-item-enable (menu command label help enable)
  "Add COMMAND to MENU annotated with LABEL and properties HELP, ENABLE."
  `(define-key-after ,menu [,command]
     '(menu-item ,label ,command
                 :help ,help
                 :enable ,enable)))

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
  "Generate context menu label with region string prepended by PREFIX."
  (let* ((start (region-beginning))
        (end (region-end))
        (buf "")
        (max 25)
        (size (abs (- start end))))
    (if (> size max)
        (setq buf (concat prefix " “"(buffer-substring start (+ max start)) "…”"))
      (setq buf (concat prefix " “" (buffer-substring start end) "”")))
    buf))

(defun cc/context-menu-last-word-in-region (prefix)
  "Generate context menu label with last word in region prepended by PREFIX."
  (let*  ((start (region-beginning))
         (end (region-end))
         (buf (buffer-substring start end))
         (last-word (car (last (split-string buf " ")))))
    (concat prefix " “" last-word "”")))

(defun cc/org-stored-links-p ()
  "Predicate if `org-stored-links' is populated.\n\n
Return t if populated, nil otherwise."
  (if (> (length org-stored-links) 0)
      t
    nil))

(provide 'cc-context-menu-macros)
