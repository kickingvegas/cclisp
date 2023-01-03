;; ObjC Mode

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))


(add-hook 'objc-mode-hook (lambda()
                            (setq c-basic-offset 4)
                            (setq-default indent-tabs-mode nil)))

