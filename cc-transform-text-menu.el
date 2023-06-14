;; Transform Text

(require 'cc-context-menu-macros)

(defvar cc/transform-text-menu (make-sparse-keymap "Transform Text")
  "Keymap for Transform Text submenu.")

(cc/add-first-context-menu-item cc/transform-text-menu
                                upcase-region
                                "Make Upper Case"
                                "Convert selected region to upper case")

(cc/add-context-menu-item cc/transform-text-menu
                          downcase-region
                          "Make Lower Case"
                          "Convert selected region to lower case")

(cc/add-context-menu-item cc/transform-text-menu
                          capitalize-region
                          "Capitalize"
                          "Convert the selected region to capitalized form")

(provide 'cc-transform-text-menu)
