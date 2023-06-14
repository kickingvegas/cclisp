;; repeat-mode

(defvar vifon/buffer-nextprev-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    #'next-buffer)
    (define-key map (kbd "<right>") #'next-buffer)
    (define-key map (kbd "<down>")  #'previous-buffer)
    (define-key map (kbd "<left>")  #'previous-buffer)
    map))
;;(put 'next-buffer     'repeat-map 'vifon/buffer-nextprev-repeat-map)
;;(put 'previous-buffer 'repeat-map 'vifon/buffer-nextprev-repeat-map)

(defun repeatize (keymap)
  "Add `repeat-mode' support to a KEYMAP."
  (map-keymap
   (lambda (_key cmd)
     (when (symbolp cmd)
       (put cmd 'repeat-map keymap)))
   (symbol-value keymap)))

(defvar cc/org-header-navigation-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p")    #'org-previous-visible-heading)
    (define-key map (kbd "n")  #'org-next-visible-heading)
    map))

(repeatize 'vifon/buffer-nextprev-repeat-map)
(repeatize 'cc/org-header-navigation-repeat-map)

(repeat-mode)

(provide 'cc-repeat-mode)
