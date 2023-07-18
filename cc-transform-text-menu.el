;; Transform Text

(require 'cc-context-menu-macros)

;; (defvar cc/transform-text-menu (make-sparse-keymap "Transform Text")
;;   "Keymap for Transform Text submenu.")

(easy-menu-define cc/transform-text-menu nil
  "Keymap for Transform Text submenu"
  '("Transform Text"
    :visible (region-active-p)      
    ["Make Upper Case" upcase-region
     :enable (region-active-p)
     :help "Convert selected region to upper case"]
    ["Make Lower Case" downcase-region
     :enable (region-active-p)     
     :help "Convert selected region to lower case"]    
    ["Capitalize" capitalize-region
     :enable (region-active-p)     
     :help "Convert the selected region to capitalized form"]))

(provide 'cc-transform-text-menu)
