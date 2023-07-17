;; Find Menu

(require 'cc-context-menu-macros)

(easy-menu-define cc/region-operations-menu nil
  "Keymap for Region Operations submenu."
  '("Operate on Region"
    :visible (region-active-p)
    ["Search with Google" google-this-noconfirm
     :enable (region-active-p)
     :label (cc/context-menu-label "Search with Google")
     :help "Search Google with selected region"]

    ["Translate" google-translate-smooth-translate
     :enable (region-active-p)
     :label (concat (cc/context-menu-label "Translate") "…")
     :help "Translate selected region with Google Translate"]

    ["Upload to Webpaste" webpaste-paste-region
     :enable (region-active-p)
     :label (cc/context-menu-label "Upload to Webpaste")
     :help "Upload selected region to paste service leaving \
link in the clipboard"]

    ["Start Speaking" cc/say-region
     :enable (region-active-p)
     :help "Start speaking selected region"]

    ["Call" cc/call-nanp-phone-number
     :enable (region-active-p)     
     :label (cc/context-menu-label "Call")
     :visible (cc/nanp-phone-number-p)
     :help "Call phone number"]
    
    ["Open in Apple Maps" cc/open-region-in-apple-maps
     :enable (region-active-p)     
     :label (cc/context-menu-label "Open in Apple Maps")
     :visible (not (cc/nanp-phone-number-p))
     :help "Open in Apple Maps"]))
 
(provide 'cc-region-operations-menu)

