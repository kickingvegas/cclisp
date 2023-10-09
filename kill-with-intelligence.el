;; kill-with-intelligence.el --- Making buffer killing suck less. -*- lexical-binding: t -*-
;;
;;; Commentary:
;; Original function at the time of this writing is at:
;; https://github.com/emacs-mirror/emacs/blob/3907c884f03cf5f2a09696bda015b1060c7111ba/lisp/simple.el#L10980
;;
;;; Code:

(defun ct/kill-buffer--possibly-save--advice (original-function buffer &rest args)
  "Ask user in the minibuffer whether to save before killing.

Replaces `kill-buffer--possibly-save' as advice, so
ORIGINAL-FUNCTION is unused and never delegated to. Its first
parameter is the buffer, which is the `car' or ARGS."
  (let ((response
         (car
          (read-multiple-choice
           (format "Buffer %s modified."
                   (buffer-name))
           '((?s "save" "save then kill buffer")
             (?d "discard" "kill buffer without saving")
             (?c "cancel" "exit without doing anything"))
           nil nil (and (not use-short-answers)
                        (not (use-dialog-box-p)))))))
    (cond ((= response ?s)
           (with-current-buffer buffer (save-buffer))
           t)
          ((= response ?d)
           t)
          ((= response ?c)
           nil)
          )))

(advice-add 'kill-buffer--possibly-save :around #'ct/kill-buffer--possibly-save--advice)

;;;

(provide 'kill-with-intelligence)
;; kill-with-intelligence.el ends here
