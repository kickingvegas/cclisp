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

    (set-face-attribute 'org-date nil
                        :foreground "purple")
    
    (set-face-attribute 'org-hide nil
                        :foreground "white"))

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

    (set-face-attribute 'org-date nil
                        :foreground "dark gray")
        
    (set-face-attribute 'org-hide nil
                        :foreground "black"))

(defun cc/light-mode-man ()
  (interactive)
  (set-face-attribute 'Man-overstrike nil
                      :foreground "dark slate blue")
  
  (set-face-attribute 'Man-underline nil
                      :foreground "MediumBlue"))

(defun cc/dark-mode-man ()
  (interactive)
  (set-face-attribute 'Man-overstrike nil
                      :foreground "white")

  (set-face-attribute 'Man-underline nil
                      :foreground "chartreuse"))

(defun cc/light-mode-elfeed ()
  (interactive)
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :foreground "blue")
  
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground "black")

  (set-face-attribute 'message-header-subject nil
                      :foreground "navy blue")

  (set-face-attribute 'message-header-to nil
                      :foreground "MidnightBlue"))

(defun cc/dark-mode-elfeed ()
  (interactive)
  (set-face-attribute 'elfeed-search-unread-title-face nil
                      :foreground "deep sky blue")
  
  (set-face-attribute 'elfeed-search-title-face nil
                      :foreground "white")

  (set-face-attribute 'message-header-subject nil
                      :foreground "green yellow")

  (set-face-attribute 'message-header-to nil
                      :foreground "cyan"))


(defun cc/morning ()
  (interactive)
  (cc/refresh-header-timestamps)
  (cc/start))

(defun cc/night ()
  (interactive)
  (cc/dark-mode))

(defun cc/reconfigure-nsappearance ()
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    (if (string-equal appearance "NSAppearanceNameDarkAqua")
        (cc/dark-mode)
      (cc/light-mode))))

(add-hook 'mac-effective-appearance-change-hook 'cc/reconfigure-nsappearance)

(provide 'cc-appearance)
