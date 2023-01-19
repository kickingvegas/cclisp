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
(defvar cc/org-emphasize-menu (make-sparse-keymap "Org Emphasize"))

(define-key cc/org-emphasize-menu [org-emphasize-bold]
  '(menu-item "Bold" cc/org-emphasize-bold
              :help "Bold"))

(define-key-after cc/org-emphasize-menu [org-emphasize-italic]
  '(menu-item "Italic" cc/org-emphasize-italic
              :help "Italic"))

(define-key-after cc/org-emphasize-menu [org-emphasize-code]
  '(menu-item "Code" cc/org-emphasize-code
              :help "Code"))

(define-key-after cc/org-emphasize-menu [org-emphasize-underline]
  '(menu-item "Underline" cc/org-emphasize-underline
              :help "Underline"))

(define-key-after cc/org-emphasize-menu [org-emphasize-verbatim]
  '(menu-item "Verbatim" cc/org-emphasize-verbatim
              :help "Verbatim"))

(define-key-after cc/org-emphasize-menu [org-emphasize-strike-through]
  '(menu-item "Strike Through" cc/org-emphasize-strike-through
              :help "Strike through"))

(define-key-after cc/org-emphasize-menu [org-emphasize-reset]
  '(menu-item "Reset" cc/org-emphasize-reset
              :help "Remove emphasis"))

;; Markdown Emphasize
(defvar cc/markdown-emphasize-menu (make-sparse-keymap "Markdown Emphasize"))

(define-key cc/markdown-emphasize-menu [markdown-emphasize-bold]
  '(menu-item "Bold" markdown-insert-bold
              :help "Bold"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-italic]
  '(menu-item "Italic" markdown-insert-italic
              :help "Italic"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-code]
  '(menu-item "Code" markdown-insert-code
              :help "Code"))

(define-key-after cc/markdown-emphasize-menu [markdown-emphasize-strike-through]
  '(menu-item "Strike Through" markdown-insert-strike-through
              :help "Strike through"))
