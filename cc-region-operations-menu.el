;; Find Menu

(require 'cc-context-menu-macros)

(defvar cc/region-operations-menu (make-sparse-keymap "Operate on Region")
  "Keymap for Region Operations submenu.")

(cc/add-first-context-menu-item cc/region-operations-menu
                                google-this-noconfirm
                                (cc/context-menu-label "Search with Google")
                                "Search Google with selected region")

(cc/add-context-menu-item cc/region-operations-menu
                          google-translate-smooth-translate
                          (concat (cc/context-menu-label "Translate") "…")
                          "Translate selected region with Google Translate")

(cc/add-context-menu-item cc/region-operations-menu
                          webpaste-paste-region
                          (cc/context-menu-label "Upload to Webpaste")
                          "Upload selected region to paste service leaving \
link in the clipboard")

(cc/add-context-menu-item cc/region-operations-menu
                          cc/say-region
                          "Start Speaking"
                          "Start speaking selected region")

(cc/add-context-menu-item-visible cc/region-operations-menu
                                  cc/call-phone-number
                                  (cc/context-menu-label "Call")
                                  "Call phone number"
                                  (cc/phone-number-p))

(cc/add-context-menu-item-visible cc/region-operations-menu
                                  cc/open-region-in-apple-maps                                  
                                  (cc/context-menu-label "Open in Apple Maps")
                                  "Open in Apple Maps"
                                  (not (cc/phone-number-p)))


;; (define-key-after cc/region-operations-menu [cc/call-phone-number]
;;   '(menu-item (cc/context-menu-label "Call")
;;               cc/call-phone-number
;;               :help "Call phone number"
;; ;;               :visible (cc/phone-number-p)))

;; (define-key-after cc/region-operations-menu [cc/open-region-in-apple-maps]
;;   '(menu-item (cc/context-menu-label "Open in Apple Maps")
;;               cc/open-region-in-apple-maps
;;               :help "Call phone number"
;;               :visible (not (cc/phone-number-p))))

(provide 'cc-region-operations-menu)

