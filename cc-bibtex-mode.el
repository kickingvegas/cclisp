;;; cc-bibtex-mode.el ---  Bibtex Mode    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Charles Choi

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
(require 'bibtex)
(require 'replace)
(require 'hl-line)
(require 'casual-lib)

(defun cc/bibtex-previous-field ()
  (interactive)
  (forward-line -2)
  (bibtex-next-field nil))

(transient-define-prefix casual-bibtex-tmenu ()
  "Transient menu for bibtex."
  :refresh-suffixes t

  ["Casual BibTeX"
   ["Field"
    :pad-keys t
    ("c" "Copy" bibtex-copy-field-as-kill :transient nil)
    ("x" "Clear" bibtex-empty-field :transient nil)
    ("a" "Add…" bibtex-make-field :transient nil)
    ("<DEL>" "Delete" bibtex-kill-field :transient nil)
    ("o" "Remove OPT/ALT" bibtex-remove-OPT-or-ALT :transient nil)]

   ["Entry"
    :pad-keys t
    ("C" "Copy" bibtex-copy-entry-as-kill :transient nil)
    ("k" "Kill" bibtex-kill-entry :transient nil)

    ("A" "Add…" bibtex-entry :transient nil)
    ("u" "Update" bibtex-entry-update :transient nil)
    ("m" "Mark" bibtex-mark-entry :transient nil)
    ("f" "Fill" bibtex-fill-entry :transient nil)
    ("C-c" "Clean" bibtex-clean-entry :transient nil)]

   ["Yank"
    ("y" "Yank" bibtex-yank :transient t)
    ("M-y" "Yank Pop" bibtex-yank-pop :transient t)]

   ["Navigation"
    :pad-keys t
    ("p" "Previous Field" previous-line :transient t)
    ("n" "Next Field" bibtex-next-field :transient t)
    ("M-p" "Previous Entry" bibtex-previous-entry :transient t)
    ("M-n" "Next Entry" bibtex-next-entry :transient t)]

   [""
    ("<" "Begin Entry" bibtex-beginning-of-entry :transient t)
    (">" "End Entry" bibtex-end-of-entry :transient t)]

   ["Misc"
    ("/" "Search…" bibtex-search-entries :transient t)
    ("j" "Jump…" bibtex-search-entry :transient t)
    ("." "Xref…" bibtex-search-crossref :transient t)
    ("s" "Sort" bibtex-sort-buffer :transient t)
    ("N" "Narrow" bibtex-narrow-to-entry
     :if-not buffer-narrowed-p
     :transient nil)
    ("w" "Widen" widen
     :if buffer-narrowed-p
     :transient nil)
    ("O" "Occur…" occur)]]

  [:class transient-row
          (casual-lib-quit-one)
          ("RET" "Edit" transient-quit-all)
          ("U" "Undo" undo :transient t)
          (casual-lib-quit-all)])


(add-hook 'bibtex-mode-hook 'hl-line-mode)

(keymap-set bibtex-mode-map "C-o" #'casual-bibtex-tmenu)
(keymap-set bibtex-mode-map "<TAB>" #'bibtex-next-field)
(keymap-set bibtex-mode-map "<backtab>" #'previous-line)

(keymap-set bibtex-mode-map "C-n" #'bibtex-next-field)
(keymap-set bibtex-mode-map "M-n" #'bibtex-next-entry)
(keymap-set bibtex-mode-map "M-p" #'bibtex-previous-entry)

(keymap-set bibtex-mode-map "<prior>" #'bibtex-previous-entry)
(keymap-set bibtex-mode-map "<next>" #'bibtex-next-entry)

(keymap-set bibtex-mode-map "C-c C-o" #'bibtex-url)


(provide 'cc-bibtex-mode)
;;; cc-bibtex-mode.el ends here
