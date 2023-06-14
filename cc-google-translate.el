(require 'google-translate)
(require 'google-translate-smooth-ui)
;;(global-set-key "\C-ct" 'google-translate-smooth-translate)

(setq google-translate-translation-directions-alist
      '(("ko" . "en") ("en" . "ko")))

(provide 'cc-google-translate)
