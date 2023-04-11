;; Transform Text
(defvar cc/transform-text-menu (make-sparse-keymap "Transform Text"))

(define-key cc/transform-text-menu [tranform-text-uppercase]
  '(menu-item "Make Upper Case" upcase-region
              :help "Upper case region"))

(define-key-after cc/transform-text-menu [tranform-text-lowercase]
  '(menu-item "Make Lower Case" downcase-region
              :help "Lower case region"))

(define-key-after cc/transform-text-menu [tranform-text-capitalize]
  '(menu-item "Capitalize" capitalize-region
              :help "Capitalize region"))
