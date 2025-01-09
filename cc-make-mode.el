;;; cc-make-mode.el --- makefile-mode configuration   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Charles Choi

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

;;; Code:
(require 'make-mode)
(require 'compile)
(require 'casual-lib)

(keymap-set makefile-mode-map "<f9>" #'compile)
(keymap-set makefile-mode-map "C-6" #'imenu)

;;(add-hook 'makefile-mode-hook #'makefile-gmake-mode)

(add-hook 'makefile-mode-hook #'imenu-add-menubar-index)
(add-hook 'makefile-mode-hook
          (lambda ()
            (setq-local imenu-auto-rescan t)
            (setq-local imenu-sort-function #'imenu--sort-by-name)))

(transient-define-prefix casual-make-tmenu ()
  "Make mode menu."

  ["Make"
   :description (lambda () (format "Make: %s" major-mode))
   ["Edit"
    :inapt-if (lambda () (if buffer-read-only t nil))
    ("\\" "Backslash region" makefile-backslash-region)
    (";" "Comment region" comment-region)
    (":" "Insert target…" makefile-insert-target-ref)
    ("f" "Insert function…" makefile-insert-gmake-function)
    ("b" "Insert using browser" makefile-switch-to-browser)
    ("a" "Automatic Variables›" casual-make-automatic-variables-tmenu)]

   ["Pickup as targets"
    ("E" "Everything" makefile-pickup-everything)
    ("F" "Filenames" makefile-pickup-filenames-as-targets)]

   ["Misc"
    ("c" "Compile…" compile)
    ("o" "Overview" makefile-create-up-to-date-overview)
    ("m" "Makefile Type›" casual-make-mode-select-tmenu)]

   ["Navigate"
    ("i" "imenu…" imenu :transient t)
    ("p" "Previous" makefile-previous-dependency :transient t)
    ("n" "Next" makefile-next-dependency :transient t)]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("RET" "Exit Menu" transient-quit-all)])

(transient-define-prefix casual-make-mode-select-tmenu ()
  "Make mode menu."

  ["Make Mode Select"
   ("a" "automake" makefile-automake-mode)
   ("b" "BSD make" makefile-bsdmake-mode)
   ("g" "GNU make" makefile-gmake-mode)
   ("i" "imake" makefile-imake-mode)
   ("m" "make" makefile-mode)
   ("e" "makepp" makefile-makepp-mode)]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(transient-define-prefix casual-make-browser-tmenu ()
  "Makefile browser menu.

The Makefile “browser” is actually an editing interface to insert
dependencies into a Makefile. It is *not* a navigation interface
for dependencies. For navigation, instead use `imenu'.

Given that clarification, the “browser” provides a selection
interface of dependencies in a separate buffer. Use the binding
\\[makefile-browser-toggle] to mark a dependency as selected. The
binding \\[makefile-browser-insert-selection] will insert the
selected dependencies at the current point of the current
Makefile."

  ["Makefile Browser"
   ["Selection"
    ("SPC" "Toggle" makefile-browser-toggle :transient t)]

   ["Navigation"
    ("p" "Previous" makefile-browser-previous-line :transient t)
    ("n" "Next" makefile-browser-next-line :transient t)]

   ["Insert"
    ("i" "Insert selected" makefile-browser-insert-selection :transient t)
    ("c" "Insert continuation" makefile-browser-insert-continuation :transient t)
    ("I" "Insert selected and quit" makefile-browser-insert-selection-and-quit)]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("q" "Quit Browser" makefile-browser-quit)])


(transient-define-prefix casual-make-automatic-variables-tmenu ()
  "Makefile automatic variables menu.

Menu for GNU Make automatic variables."

  ["Automatic Variables (GNU Make)"
   ["Target @"
    ("$@" "Name" casual-make-insert-target-name)
    ("$(@D)" "Directory" (lambda () (interactive) (insert "$(@D)")))
    ("$(@F)" "File" (lambda () (interactive) (insert "$(@F)")))]

   ["Implicit Stem *"
    ("$*" "Stem" (lambda () (interactive) (insert "$*")))
    ("$(*D)" "Directory" (lambda () (interactive) (insert "$(*D)")))
    ("$(*F)" "File" (lambda () (interactive) (insert "$(*F)")))]

   ["Archive %"
    ("$%" "Archive" (lambda () (interactive) (insert "$%")))
    ("$(%D)" "Directory" (lambda () (interactive) (insert "$(%D)")))
    ("$(%F)" "File" (lambda () (interactive) (insert "$(%F)")))]]

  ["Prerequisites"
   ["First <"
    ("$<" "First" (lambda () (interactive) (insert "$<")))
    ("$(<D)" "Directory" (lambda () (interactive) (insert "$(<D)")))
    ("$(<F)" "File" (lambda () (interactive) (insert "$(<F)")))]

   ["Newer than Target ?"
    ("$?" "All" (lambda () (interactive) (insert "$?")))
    ("$(?D)" "Directory" (lambda () (interactive) (insert "$(?D)")))
    ("$(?F)" "File" (lambda () (interactive) (insert "$(?F)")))]

   ["Normalized ^"
    ("$^" "All" (lambda () (interactive) (insert "$^")))
    ("$(^D)" "Directory" (lambda () (interactive) (insert "$(^D)")))
    ("$(^F)" "File" (lambda () (interactive) (insert "$(^F)")))]

   ["Include Duplicates +"
    ("$+" "All" (lambda () (interactive) (insert "$+")))
    ("$(+D)" "Directory" (lambda () (interactive) (insert "$(+D)")))
    ("$(+F)" "File" (lambda () (interactive) (insert "$(+F)")))]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)
   ("RET" "Dismiss" casual-lib-quit-all)
   ("i" "Info" (lambda () (interactive) (info "(make) Automatic Variables")))])

(defun casual-make-insert-target-name ()
  "Insert the automatic make variable '$@' for the target name.

This is typically the filename of the target rule.

If the target is an archive member, then '$@' is the name of the
archive file.

For more info, refer to info node `(make) Automatic Variables'."
  (interactive)
  (insert "$@"))

(keymap-set makefile-mode-map "M-m" #'casual-make-tmenu)
(keymap-set makefile-browser-map "M-m" #'casual-make-browser-tmenu)

(provide 'cc-make-mode)
;;; cc-make-mode.el ends here
