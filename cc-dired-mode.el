;;; cc-dired-mode.el --- Dired Customization -*- lexical-binding: t -*-
;; dired-mode

;;; Commentary:
;;

;;; Code:
(require 'dired)
(require 'transient)
(require 'cclisp)
(require 'helm)
(require 'easymenu)

(with-eval-after-load 'dired
  (require 'dired-x))
(add-hook 'dired-mode-hook 'hl-line-mode)
(add-hook 'dired-mode-hook 'context-menu-mode)

(defun cc/dired-inspect-object ()
  "WIP: inspect Dired object."
  (interactive)
  (if (dired-get-subdir)
      ;; nop
      (message "subdir")
    (let ((fname (dired-get-filename)))
      (cond
       ((not (derived-mode-p 'dired-mode))
        ;; nop
        (message "Not in dired mode."))

       ((file-symlink-p fname)
        ;; nop
        (message fname))

       ((file-directory-p fname)
        ;; open in subdir
        (message "directory")
        (dired-maybe-insert-subdir fname))

       ((file-regular-p fname)
        ;; mark file
        (message fname)
        (dired-find-file))

       (t
        (message "undefined"))))))

;; (add-hook 'dired-mode-hook (lambda ()
;;                              (define-key dired-mode-map (kbd "<mouse-1>") 'cc/dired-inspect-object)))

(transient-define-prefix cc/dired-sort-by ()
  "Transient menu to sort Dired buffer by different criteria.
This function requires GNU ls from coreutils installed."
  :value '("--human-readable"
           "--group-directories-first"
           "--time-style=long-iso")
  [["Arguments"
    ("-a" "all" "--all")
    ("g" "group directories first" "--group-directories-first")
    ("-r" "reverse" "--reverse")
    ("-h" "human readable" "--human-readable")
    ("t" "time style" "--time-style="
     :choices ("full-iso" "long-iso" "iso" "locale"))]

   ["Sort By"
    ("n"
     "Name"
     (lambda () (interactive)
       (cc/--dired-sort-by :name
                           (transient-args transient-current-command)))
     :transient nil)
    ("k"
     "Kind"
     (lambda () (interactive)
       (cc/--dired-sort-by :kind
                           (transient-args transient-current-command)))
     :transient nil)
    ("l"
     "Date Last Opened"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-last-opened
                           (transient-args transient-current-command)))
     :transient nil)
    ("a"
     "Date Added"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-added
                           (transient-args transient-current-command)))
     :transient nil)
    ("m"
     "Date Modified"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-modified
                           (transient-args transient-current-command)))
     :transient nil)
    ("M"
     "Date Metadata Changed"
     (lambda () (interactive)
       (cc/--dired-sort-by :date-metadata-changed
                           (transient-args transient-current-command)))
     :transient nil)
    ("v"
     "Version"
     (lambda () (interactive)
       (cc/--dired-sort-by :version
                           (transient-args transient-current-command)))
     :transient nil)
    ("s"
     "Size"
     (lambda () (interactive)
       (cc/--dired-sort-by :size
                           (transient-args transient-current-command)))
     :transient nil)]])

(defun cc/--dired-sort-by (criteria &optional prefix-args)
  "Sort current Dired buffer according to CRITERIA and PREFIX-ARGS.
This function will invoke `dired-sort-other' with arguments provided by
`cc/dired-sort-by-transient-menu'.

CRITERIA is a keyword of which the following are supported:
(:name :kind :date-last-opened :date-added :date-modified
 :date-metadata-changed :version :size)

PREFIX-ARGS is a list returned from the function
(`transient-args' `transient-current-command')

This function requires GNU ls from coreutils installed."
  (let ((arg-list (list "-l")))
    (if prefix-args
        (nconc arg-list prefix-args)
      (nconc arg-list (list "--group-directories-first"
                            "--human-readable"
                            "--time-style=long-iso")))
    (cond
     ((eq criteria :name)
      (message "Sorting by name"))

     ((eq criteria :kind)
      (message "Sorting by kind")
      (push "--sort=extension" arg-list))

     ((eq criteria :date-last-opened)
      (message "Sorting by date last opened")
      (push "--sort=time" arg-list)
      (push "--time=access" arg-list))

     ((eq criteria :date-added)
      (message "Sorting by date added")
      (push "--sort=time" arg-list)
      (push "--time=creation" arg-list))

     ((eq criteria :date-modified)
      (message "Sorting by date modified")
      (push "--sort=time" arg-list)
      (push "--time=modification" arg-list))

     ((eq criteria :date-metadata-changed)
      (message "Sorting by date metadata changed")
      (push "--sort=time" arg-list)
      (push "--time=status" arg-list))

     ((eq criteria :version)
      (message "Sorting by version")
      (push "--sort=version" arg-list))

     ((eq criteria :size)
      (message "Sorting by size")
      (push "-S" arg-list))

     (t
      (message "Default sorting by name")))

    (dired-sort-other (mapconcat 'identity arg-list " "))))

(easy-menu-define cc/dired-sort-menu nil
  "Keymap for Dired sort by submenu."
  '("Sort By"
    ["Name"
     (lambda () (interactive) (cc/--dired-sort-by :name))]
    ["Kind"
     (lambda () (interactive) (cc/--dired-sort-by :kind))]
    ["Date Last Opened"
     (lambda () (interactive) (cc/--dired-sort-by :date-last-opened))]
    ["Date Added"
     (lambda () (interactive) (cc/--dired-sort-by :date-added))]
    ["Date Modified"
     (lambda () (interactive) (cc/--dired-sort-by :date-modified))]
    ["Date Metadata Changed"
     (lambda () (interactive) (cc/--dired-sort-by :date-metadata-changed))]
    ["Version"
     (lambda () (interactive) (cc/--dired-sort-by :version))]
    ["Size"
     (lambda () (interactive) (cc/--dired-sort-by :size))]))

(add-hook
 'dired-mode-hook
 (lambda ()
   (define-key dired-mode-map (kbd "M-o") 'dired-omit-mode)
   (define-key dired-mode-map (kbd "C-o") 'cc/meta-search)
   (define-key dired-mode-map (kbd "E") 'wdired-change-to-wdired-mode)
   (define-key dired-mode-map (kbd "s") 'cc/dired-sort-by)
   (define-key dired-mode-map (kbd "j") 'helm-find-files)
   (setq-local mouse-1-click-follows-link 'double)
   ;;(define-key dired-mode-map (kbd "A-M-<mouse-2>") 'cc/dired-inspect-object)
   (define-key dired-mode-map (kbd "A-M-<mouse-1>") 'browse-url-of-dired-file)))

(provide 'cc-dired-mode)
;;; cc-dired-mode.el ends here
