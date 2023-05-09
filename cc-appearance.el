(defun cc/light-mode ()
    (interactive)
    (set-face-attribute 'minibuffer-prompt nil
                        :foreground "dark magenta")
    
    (set-face-attribute 'org-table nil
                        :foreground "Blue1")

    (set-face-attribute 'org-scheduled-previously nil
                        :foreground "#2255ff")

  
    (set-face-attribute 'comint-highlight-prompt nil
                        :foreground "dark magenta")
    
    (set-face-attribute 'org-document-title nil
                        :foreground "midnight blue")

    (set-face-attribute 'org-document-info nil
                        :foreground "midnight blue")
    
    (set-face-attribute 'org-hide nil
                        :foreground "white"))


(defun cc/light-mode-man ()
  (interactive)
  (set-face-attribute 'Man-overstrike nil
                      :foreground "dark slate blue")
  
  (set-face-attribute 'Man-underline nil
                      :foreground "MediumBlue"))

(defun cc/dark-mode ()
    (interactive)
    (set-face-attribute 'minibuffer-prompt nil
                        :foreground "orange")
    
    (set-face-attribute 'org-table nil
                        :foreground "#00ff22")

    (set-face-attribute 'org-scheduled-previously nil
                        :foreground "light sky blue")


    (set-face-attribute 'comint-highlight-prompt nil
                        :foreground "cyan")

    (set-face-attribute 'org-document-title nil
                        :foreground "chartreuse")

    (set-face-attribute 'org-document-info nil
                        :foreground "chartreuse")
        
    (set-face-attribute 'org-hide nil
                        :foreground "black"))

(defun cc/dark-mode-man ()
  (interactive)
  (set-face-attribute 'Man-overstrike nil
                      :foreground "white")

  (set-face-attribute 'Man-underline nil
                      :foreground "chartreuse"))
  