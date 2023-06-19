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


(define-key-after cc/region-operations-menu [cc/call-phone-number]
  '(menu-item (cc/context-menu-label "Call")
              cc/call-phone-number
              :help "Call phone number"
              :visible (cc/phone-number-p)))

(provide 'cc-region-operations-menu)

