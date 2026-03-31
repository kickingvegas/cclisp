;;; cc-emacs-lisp-mode.el --- Elisp Customization -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Charles Choi

;; Author: Charles Choi <kickingvegas@gmail.com>

;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

(require 'elisp-mode)
;;(require 'paredit)
(require 'flycheck)
(require 'edebug)
(require 'cclisp)
(require 'calle24-edebug)
(require 'casual-elisp)

;;; Code:

;;(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook #'prettify-symbols-mode)


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local imenu-generic-expression
                        (push
                         (list "Structs" "(cl-defstruct (*\\([[:alnum:]-]*\\)" 1)
                         imenu-generic-expression))

            (setq-local imenu-generic-expression
                        (push
                         (list "Transient Prefixes" "(transient-define-prefix \\([[:alnum:]-]*\\)" 1)
                         imenu-generic-expression))

            (setq-local imenu-generic-expression
                        (push
                         (list "Transient Groups" "(transient-define-group \\([[:alnum:]-]*\\)" 1)
                         imenu-generic-expression))))

(keymap-set emacs-lisp-mode-map "M-[" #'backward-sexp)
(keymap-set emacs-lisp-mode-map "M-]" #'forward-sexp)
(keymap-set emacs-lisp-mode-map "<f6>" #'cc/describe-function-point-is-in)
(keymap-set emacs-lisp-mode-map "M-j" #'fill-paragraph)
(keymap-set emacs-lisp-mode-map "M-n" #'cc/next-sexp)
(keymap-set emacs-lisp-mode-map "M-p" #'backward-sexp)
(keymap-set emacs-lisp-mode-map "C-<up>" #'backward-up-list)
(keymap-set emacs-lisp-mode-map "C-<down>" #'down-list)
(keymap-set emacs-lisp-mode-map "C-<left>" #'backward-sexp)
(keymap-set emacs-lisp-mode-map "C-<right>" #'cc/next-sexp)

(keymap-set emacs-lisp-mode-map "M-b" #'backward-sexp)
(keymap-set emacs-lisp-mode-map "M-f" #'cc/next-sexp)
(keymap-set emacs-lisp-mode-map "C-M-b" #'backward-word)
(keymap-set emacs-lisp-mode-map "C-M-f" #'forward-word)

(keymap-set emacs-lisp-mode-map "M-<prior>" #'cc/backward-page-at-top)
(keymap-set emacs-lisp-mode-map "M-<next>" #'cc/forward-page-at-top)

(keymap-set emacs-lisp-mode-map "C-<prior>" #'pages-previous-page)
(keymap-set emacs-lisp-mode-map "C-<next>" #'pages-next-page)
(keymap-set emacs-lisp-mode-map "M-RET" #'cc/line-feed)

(transient-define-prefix cc/edebug-tmenu ()
  :refresh-suffixes t
  ["Debug"
   :class transient-row
   :if edebug-mode-p
   ("h" "here" edebug-goto-here :transient nil)
   ("f" "→" edebug-forward-sexp :transient nil)
   ("i" "→(" edebug-step-in :transient nil)
   ("o" "→)" edebug-step-out :transient nil)
   ("e" "❯…" edebug-eval-expression :transient t)
   ("r" "∴" edebug-previous-result :transient t)
   ("d" "≣" edebug-pop-to-backtrace :transient t)]

  ["Actions"
   :class transient-row
   ("I" "🔬 Instrument" cc/instrument-function :transient nil)
   ("R" "▶️ Eval/Run" eval-defun :transient nil)
   ("E" "🔎 Watch List" edebug-visit-eval-list :if edebug-mode-p :transient nil)
   ("S" "⏹️ Stop" edebug-stop :if edebug-mode-p :transient nil)]

  ["Navigate"
   :class transient-row
   ("C-b" "←" backward-char :transient t)
   ("C-f" "→" forward-char :transient t)
   ("C-p" "↑" previous-line :transient t)
   ("C-n" "↓" next-line :transient t)
   ("M-[" "(←" backward-sexp :transient t)
   ("M-]" "→)" forward-sexp :transient t)]

  [["Modes"
    :description (lambda () (format "Modes (now %s)" edebug-initial-mode))
    :pad-keys t
    ("SPC" "Step" edebug-step-mode :transient nil)
    ("g" "Go" edebug-go-mode :transient nil)
    ("n" "Next" edebug-next-mode :transient nil)
    ("c" "Continue" edebug-continue-mode :transient nil)
    ("t" "Trace" edebug-trace-mode :transient nil)
    ("T" "Trace Fast" edebug-Trace-fast-mode :transient nil)
    ("G" "Go Nonstop" edebug-Go-nonstop-mode :transient nil)
    ("m" "Set Mode…" edebug-set-initial-mode :transient nil)]

   ["Breakpoint"
    :pad-keys t
    ("s" "Set" edebug-set-breakpoint :transient t)
    ("u" "Unset" edebug-unset-breakpoint :transient t)
    ("U" "Unset All" edebug-unset-breakpoints :transient t)
    ("D" "Disable" edebug-toggle-disable-breakpoint :transient t)
    ("x" "Set Conditional…" edebug-set-conditional-breakpoint :transient nil)
    ("B" "Move to next" edebug-next-breakpoint :transient t)]

   ["Control"
    :pad-keys t
    :if edebug-mode-p
    ("a" "Abort One Level" abort-recursive-edit :transient nil)
    ("q" "Quit Edebug" top-level :transient nil)
    ("Q" "Quit Edebug Nonstop" edebug-top-level-nonstop :transient nil)]

   ["Views"
    :pad-keys t
    :if edebug-mode-p
    ("v" "View" edebug-view-outside)
    ("p" "Bounce Point" edebug-bounce-point)
    ("w" "Where" edebug-where)]]

  [:class transient-row
          ("C-i" "Info" cc/open-edebug-info)
          ("C-g" "Dismiss" ignore :transient transient-quit-one)])


;; (defun cc/edebug-slow-after (_before-index after-index value)
;;   (call-interactively #'cc/edebug-tmenu)
;;   )

;; (advice-add #'edebug-slow-after :after #'cc/edebug-slow-after)

(defun edebug-mode-p ()
  (if edebug-mode t
    nil))

(defun cc/open-edebug-info ()
  (interactive)
  (other-frame-prefix)
  (info "(elisp) Edebug"))

(defun cc/instrument-function ()
  (interactive)
  (setq current-prefix-arg '(4))
  (call-interactively #'eval-defun))

;; (add-hook 'edebug-setup-hook (lambda ()
;;                                (call-interactively #'cc/edebug-tmenu)))

;;(advice-add #'edebug-set-mode :after #'cc/edebug-set-mode)
;;(advice-remove #'edebug-set-mode #'cc/edebug-set-mode)

                                        ;(advice-remove #'edebug-default-enter #'cc/edebug-default-enter)


;; (advice-add #'edebug-print-trace-after :after #'cc/edebug-print-trace-after)
;; (advice-remove #'edebug-print-trace-after #'cc/edebug-print-trace-after)

;; (defun cc/edebug-print-trace-after (msg)
;;   (call-interactively #'cc/edebug-tmenu))

;; (defun cc/edebug-set-mode (mode shortmsg msg)
;;   (call-interactively #'cc/edebug-tmenu))

;; (defun cc/edebug-slow-before (stuff)
;;   "Advise default enter"
;;   (call-interactively #'cc/edebug-tmenu))

;; (defun cc/edebug-default-enter (function args body)
;;   "Advise default enter"
;;   (call-interactively #'cc/edebug-tmenu))

;; (advice-add #'edebug-default-enter :after #'cc/edebug-default-enter)

;;(keymap-set edebug-mode-map "C-<f9>" #'cc/edebug-tmenu)
(keymap-set emacs-lisp-mode-map "<f8>" #'cc/edebug-tmenu)

(transient-define-prefix cc/edebug-watch-tmenu ()
  [["Expression"
    ("a" "Add" edebug-update-eval-list)
    ("d" "Delete" edebug-delete-eval-item)]

   ["Eval"
    ("l" "Last" edebug-eval-last-sexp)
    ("p" "Print" edebug-eval-print-last-sexp)]

   [""
    ("w" "Resume debug" edebug-where)]]

  [("C-g" "Dismiss" ignore :transient transient-quit-one)])

(keymap-set edebug-eval-mode-map "<f8>" #'cc/edebug-watch-tmenu)
(keymap-set emacs-lisp-mode-map "M-m" #'casual-elisp-tmenu)
(keymap-set emacs-lisp-mode-map "C-c m" #'casual-elisp-tmenu)


;; Calle 24 Config

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local tool-bar-map (calle24-edebug-mode-tool-bar-map))
            (let ((appearance (calle24-get-appearance)))
              (cond
               ((string= appearance "dark")
                (calle24-update-tool-bar-appearance t))
               ((string= appearance "light")
                (calle24-update-tool-bar-appearance))
               (t (calle24-update-tool-bar-appearance))))))

(add-hook 'edebug-eval-mode-hook
          (lambda ()
            (setq-local tool-bar-map (calle24-edebug-eval-mode-tool-bar-map))
            (let ((appearance (calle24-get-appearance)))
              (cond
               ((string= appearance "dark")
                (calle24-update-tool-bar-appearance t))
               ((string= appearance "light")
                (calle24-update-tool-bar-appearance))
               (t (calle24-update-tool-bar-appearance))))))

(add-hook 'edebug-eval-mode-hook #'window-tool-bar-mode)


(provide 'cc-emacs-lisp-mode)
;;; cc-emacs-lisp-mode.el ends here
