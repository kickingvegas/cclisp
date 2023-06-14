;;

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

(provide 'cc-context-menu-macros)
