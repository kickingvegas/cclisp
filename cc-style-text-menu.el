;;

(require 'cc-context-menu-macros)

(defun cc/org-emphasize-bold ()
  (interactive)
  (org-emphasize ?*))

(defun cc/org-emphasize-italic ()
  (interactive)
  (org-emphasize ?/))

(defun cc/org-emphasize-code ()
  (interactive)
  (org-emphasize ?~))

(defun cc/org-emphasize-underline ()
  (interactive)
  (org-emphasize ?_))

(defun cc/org-emphasize-verbatim ()
  (interactive)
  (org-emphasize ?=))

(defun cc/org-emphasize-strike-through ()
  (interactive)
  (org-emphasize ?+))

(defun cc/org-emphasize-reset ()
  ;; this won't work when org-hide-emphasis-markers is turned on.
  (interactive)
  (org-emphasize ?\s))

;; Org Emphasize
(defvar cc/org-emphasize-menu (make-sparse-keymap "Org Emphasize")
  "Keymap for Org Emphasize submenu.")

(cc/add-first-context-menu-item cc/org-emphasize-menu
				cc/org-emphasize-bold
				"Bold"
				"Bold selected region")

(cc/add-context-menu-item cc/org-emphasize-menu
			  cc/org-emphasize-italic
			  "Italic"
			  "Italic selected region")

(cc/add-context-menu-item cc/org-emphasize-menu
			  cc/org-emphasize-code
			  "Code"
			  "Code selected region")

(cc/add-context-menu-item cc/org-emphasize-menu
			  cc/org-emphasize-underline
			  "Underline"
			  "Underline selected region")

(cc/add-context-menu-item cc/org-emphasize-menu
			  cc/org-emphasize-verbatim
			  "Verbatim"
			  "Verbatim selected region")

(cc/add-context-menu-item cc/org-emphasize-menu
			  cc/org-emphasize-strike-through
			  "Strike through"
			  "Strike through selected region")

;; (cc/add-context-menu-item cc/org-emphasize-menu
;;                       org-emphasize-reset
;;                       cc/org-emphasize-reset
;;                       "Reset"
;;                       "Remove emphasis on selected region")

;; Markdown Emphasize
(defvar cc/markdown-emphasize-menu (make-sparse-keymap "Markdown Emphasize")
  "Keymap for Markdown Emphasize submenu.")

(cc/add-first-context-menu-item cc/markdown-emphasize-menu
				markdown-insert-bold
				"Bold"
				"Bold selected region")

(cc/add-context-menu-item cc/markdown-emphasize-menu
			  markdown-insert-italic
			  "Italic"
			  "Italic selected region")

(cc/add-context-menu-item cc/markdown-emphasize-menu
			  markdown-insert-code
			  "Code"
			  "Code selected region")

(cc/add-context-menu-item cc/markdown-emphasize-menu
			  markdown-insert-strike-through
			  "Strike through"
			  "Strike through selected region")

(provide 'cc-style-text-menu)
