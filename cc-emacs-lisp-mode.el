;;; cc-emacs-lisp-mode.el --- Elisp Customization -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Charles Choi

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
(require 'paredit)
(require 'flycheck)
(require 'edebug)
(require 'cclisp)
(require 'calle24)

;;; Code:

(add-hook 'emacs-lisp-mode #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode #'flycheck-mode)

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

(transient-define-prefix cc/elisp-tmenu ()
  ["Emacs Lisp"
   ["Evaluate"
    ("x" "Last Sexp" eval-last-sexp)
    ("L" "Buffer or Region" elisp-eval-region-or-buffer)
    ("d" "Defun" eval-defun)]

   ["Byte-Compile"
    ("B" "File…" elisp-byte-compile-file)
    ("b" "Buffer" elisp-byte-compile-buffer)
    ("D" "Directory" byte-recompile-directory)]

   ["Checkdoc"
    ("c" "Checkdoc…" checkdoc)]

   ["Find"
    ("l" "Library…" find-library)
    ("v" "Variable…" find-variable)
    ("f" "Function…" find-function)]]

  ["Navigate"
   :pad-keys t
   [("<left>" "←" backward-char :transient t)
    ("C-<left>" "(←" backward-sexp :transient t)]
   [("<right>" "→" forward-char :transient t)
    ("C-<right>" "→)" cc/next-sexp :transient t)]
   [("<up>" "↑" previous-line :transient t)
    ("C-<up>" "(↰" backward-up-list :transient t)]
   [("<down>" "↓" next-line :transient t)
    ("C-<down>" "⤵(" down-list :transient t)]]

  [("RET" "Dismiss" transient-quit-all)])

(keymap-set emacs-lisp-mode-map "M-m" #'cc/elisp-tmenu)
(keymap-set emacs-lisp-mode-map "C-c m" #'cc/elisp-tmenu)


(defun cc/edebug-eval-mode-tool-bar-map ()
  "Configure Edebug Tool Bar Map."
  (let ((map (make-sparse-keymap)))

    (tool-bar-local-item
     "edebug/update-eval-list"
     #'edebug-update-eval-list #'edebug-update-eval-list map
     :help "Add Symbol")

    (tool-bar-local-item
     "edebug/delete-eval-item"
     #'edebug-delete-eval-item #'edebug-delete-eval-item map
     :help "Delete Symbol")

    (tool-bar-local-item
     "edebug/eval-last-sexp"
     #'edebug-eval-last-sexp #'edebug-eval-last-sexp map
     :help "Eval Last")

    (tool-bar-local-item
     "edebug/eval-print-last-sexp"
     #'edebug-eval-print-last-sexp #'edebug-eval-print-last-sexp map
     :help "Print Eval Last")

    (tool-bar-local-item
     "edebug/where"
     #'edebug-where #'edebug-where map
     :help "Resume")

    map))

(add-hook 'edebug-eval-mode-hook
          (lambda ()
            (setq-local tool-bar-map (cc/edebug-eval-mode-tool-bar-map))))

(defun cc/edebug-mode-tool-bar-map ()
  "Configure Edebug Tool Bar Map."
  (let ((map (make-sparse-keymap)))
    ;; Edebug
    (tool-bar-local-item
     "edebug/eval-defun"
     #'eval-defun #'eval-defun map
     :visible (not (edebug-mode-p))
     :help "Go (eval-defun)")

    (tool-bar-local-item
     "edebug/goto-here"
     #'edebug-goto-here #'edebug-goto-here map
     :visible (edebug-mode-p)
     :help "Here")

    (tool-bar-local-item
     "edebug/set-mode"
     #'edebug-set-initial-mode #'edebug-set-initial-mode map
     :visible (not (edebug-mode-p))
     :help "Set Initial Mode")

    (tool-bar-local-item
     "edebug/next-mode"
     #'edebug-next-mode #'edebug-next-mode map
     :visible (edebug-mode-p)
     :help "Next Mode")

    (tool-bar-local-item
     "edebug/continue-mode"
     #'edebug-continue-mode #'edebug-continue-mode map
     :visible (edebug-mode-p)
     :help "Continue Mode")

    (tool-bar-local-item
     "edebug/trace-mode"
     #'edebug-trace-mode #'edebug-trace-mode map
     :visible (edebug-mode-p)
     :help "Trace Mode")


    (tool-bar-local-item
     "edebug/go-mode"
     #'edebug-go-mode #'edebug-go-mode map
     :visible (edebug-mode-p)
     :help "Go Mode")

    (tool-bar-local-item
     "edebug/step-mode"
     #'edebug-step-mode #'edebug-step-mode map
     :visible (edebug-mode-p)
     :help "Step Mode")

    (tool-bar-local-item
     "edebug/forward-sexp"
     #'edebug-forward-sexp #'edebug-forward-sexp map
     :visible (edebug-mode-p)
     :help "Forward sexp (Step Over)")

    (tool-bar-local-item
     "edebug/step-in"
     #'edebug-step-in #'edebug-step-in map
     :visible (edebug-mode-p)
     :help "Step in sexp")

    (tool-bar-local-item
     "edebug/step-out"
     #'edebug-step-out #'edebug-step-out map
     :visible (edebug-mode-p)
     :help "Step out sexp")

    (tool-bar-local-item
     "edebug/eval-expression"
     #'edebug-eval-expression #'edebug-eval-expression map
     :visible (edebug-mode-p)
     :help "Evaluate Expression")

    (tool-bar-local-item
     "edebug/previous-result"
     #'edebug-previous-result #'edebug-previous-result map
     :visible (edebug-mode-p)
     :help "Previous Result")

    (tool-bar-local-item
     "edebug/view-outside"
     #'edebug-view-outside #'edebug-view-outside map
     :visible (edebug-mode-p)
     :help "View Outside")

    (tool-bar-local-item
     "edebug/visit-eval-list"
     #'edebug-visit-eval-list #'edebug-visit-eval-list map
     :visible (edebug-mode-p)
     :help "Watchlist")

    ;; (define-key-after map [separator-1] menu-bar-separator)

    (tool-bar-local-item
     "edebug/set-breakpoint"
     #'edebug-set-breakpoint #'edebug-set-breakpoint map
     :visible (edebug-mode-p)
     :help "Set Breakpoint")

    (tool-bar-local-item
     "edebug/conditional-breakpoint"
     #'edebug-set-conditional-breakpoint #'edebug-set-conditional-breakpoint map
     :visible (edebug-mode-p)
     :help "Set Conditional Breakpoint")

    (tool-bar-local-item
     "edebug/next-breakpoint"
     #'edebug-next-breakpoint #'edebug-next-breakpoint map
     :visible (edebug-mode-p)
     :help "Next Breakpoint")

    (tool-bar-local-item
     "edebug/unset-breakpoint"
     #'edebug-unset-breakpoint #'edebug-unset-breakpoint map
     :visible (edebug-mode-p)
     :help "Unset Breakpoint")

    (tool-bar-local-item
     "edebug/unset-all-breakpoints"
     #'edebug-unset-breakpoints #'edebug-unset-breakpoints map
     :visible (edebug-mode-p)
     :help "Unset All Breakpoints")

    (tool-bar-local-item
     "edebug/stop"
     #'edebug-stop #'edebug-stop map
     :visible (edebug-mode-p)
     :help "Stop")

    (tool-bar-local-item
     "edebug/top-level"
     #'top-level #'top-level map
     :visible (edebug-mode-p)
     :help "Quit Edebug")

    (tool-bar-local-item
     "edebug/top-level-nonstop"
     #'edebug-top-level-nonstop #'edebug-top-level-nonstop map
     :visible (edebug-mode-p)
     :help "Quit Edebug Nonstop")

    (tool-bar-local-item
     "help"
     #'edebug-help #'edebug-help map
     :visible (edebug-mode-p)
     :help "Help")

    ;; standard tool bar items

    ;; (tool-bar-local-item
    ;;  "new"
    ;;  #'find-file
    ;;  #'find-file
    ;;  map
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "New File")

    ;; (tool-bar-local-item
    ;;  "open"
    ;;  #'menu-find-file-existing #'menu-find-file-existing map
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Open")

    ;; (tool-bar-local-item
    ;;  "diropen"
    ;;  #'dired #'dired map
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Dired")

    ;; (tool-bar-local-item
    ;;  "close"
    ;;  #'kill-this-buffer #'kill-this-buffer map
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Close")

    ;; (tool-bar-local-item
    ;;  "save"
    ;;  #'save-buffer #'save-buffer map
    ;;  :enable (buffer-modified-p)
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Save")

    ;; (tool-bar-local-item
    ;;  "undo"
    ;;  #'undo #'undo map
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Undo")

    ;; (tool-bar-local-item
    ;;  "cut"
    ;;  #'kill-region #'kill-region map
    ;;  :enable (and (use-region-p) (not buffer-read-only))
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Cut")

    ;; (tool-bar-local-item
    ;;  "copy"
    ;;  #'copy-region-as-kill #'copy-region-as-kill map
    ;;  :enable (use-region-p)
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Copy")

    ;; (tool-bar-local-item
    ;;  "paste"
    ;;  #'yank #'yank map
    ;;  :enable (and (use-region-p) (not buffer-read-only))
    ;;  :visible (not (edebug-mode-p))
    ;;  :help "Paste")


    (tool-bar-local-item-from-menu 'find-file "new" map nil
                                   :label "New File"
			           :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu 'menu-find-file-existing "open" map nil
			           :label "Open"
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu 'dired "diropen" map nil
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu 'kill-this-buffer "close" map nil
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu 'save-buffer "save" map nil
			           :label "Save"
                                   :visible (not (edebug-mode-p)))
    ;; (define-key-after (default-value 'tool-bar-map) [separator-1] menu-bar-separator)
    (tool-bar-local-item-from-menu 'undo "undo" map nil
                                   :visible (not (edebug-mode-p)))
    ;; (define-key-after (default-value 'tool-bar-map) [separator-2] menu-bar-separator)
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [cut])
			           "cut" map nil
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [copy])
			           "copy" map nil
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    (tool-bar-local-item-from-menu (lookup-key menu-bar-edit-menu [paste])
			           "paste" map nil :vert-only t
                                   :visible (not (edebug-mode-p)))
    ;; (define-key-after (default-value 'tool-bar-map) [separator-3] menu-bar-separator)
    (tool-bar-local-item-from-menu 'isearch-forward "search"
			           map nil
                                   :label "Search"
                                   :vert-only t
                                   :visible (not (edebug-mode-p)))
    map))


(add-hook 'edebug-mode-hook
          (lambda ()
            (setq-local tool-bar-map (cc/edebug-mode-tool-bar-map))))

;; TODO: handle dark mode images

(defvar cc/edebug--image-appearance-map
  '((edebug-update-eval-list . "edebug/update-eval-list")
    (edebug-delete-eval-item . "edebug/delete-eval-item")
    (edebug-eval-last-sexp . "edebug/eval-last-sexp")
    (edebug-eval-print-last-sexp . "edebug/eval-print-last-sexp")
    (edebug-where . "edebug/where")
    (eval-defun . "edebug/eval-defun")
    (edebug-goto-here . "edebug/goto-here")
    (edebug-set-initial-mode . "edebug/set-mode")
    (edebug-next-mode . "edebug/next-mode" )
    (edebug-continue-mode . "edebug/continue-mode")
    (edebug-trace-mode . "edebug/trace-mode")
    (edebug-go-mode . "edebug/go-mode")
    (edebug-step-mode . "edebug/step-mode")
    (edebug-forward-sexp . "edebug/forward-sexp")
    (edebug-step-in . "edebug/step-in")
    (edebug-step-out . "edebug/step-out")
    (edebug-eval-expression . "edebug/eval-expression")
    (edebug-previous-result . "edebug/previous-result")
    (edebug-view-outside . "edebug/view-outside")
    (edebug-visit-eval-list . "edebug/visit-eval-list")
    (edebug-set-breakpoint . "edebug/set-breakpoint")
    (edebug-set-conditional-breakpoint . "edebug/conditional-breakpoint")
    (edebug-next-breakpoint . "edebug/next-breakpoint")
    (edebug-unset-breakpoint . "edebug/unset-breakpoint")
    (edebug-unset-breakpoints . "edebug/unset-all-breakpoints")
    (edebug-stop . "edebug/stop")
    (top-level . "edebug/top-level")
    (edebug-top-level-nonstop . "edebug/top-level-nonstop")
    (edebug-help . "help"))
  "Alist map of keys used by Edebug toolbar map.")

(setq calle24--image-appearance-map (append calle24--image-appearance-map
                                            cc/edebug--image-appearance-map))

(provide 'cc-emacs-lisp-mode)
;;; cc-emacs-lisp-mode.el ends here
