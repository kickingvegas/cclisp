;;; cc-markdown-mode.el --- Markdown customizations -*- lexical-binding: t; -*-

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

;;; Code:
(require 'markdown-mode)
(require 'face-remap)
(require 'cclisp)
(require 'cc-save-hooks)
(require 'cc-style-text-menu)
(require 'org-table)
(require 'imenu)
(require 'cc-blog-utils)
(require 'casual-lib)

(defvar markdown-mode-map)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)

(add-hook 'markdown-mode-hook #'variable-pitch-mode)
;;(add-hook 'markdown-mode-hook #'markdown-toggle-markup-hiding)

(add-hook 'markdown-mode-hook #'turn-on-orgtbl)

(keymap-set markdown-mode-map "C-<up>" #'markdown-backward-same-level)
(keymap-set markdown-mode-map "C-<down>" #'markdown-forward-same-level)
(keymap-set markdown-mode-map "M-v" #'markdown-outline-previous)
(keymap-set markdown-mode-map "C-v" #'markdown-outline-next)
(keymap-set markdown-mode-map "M-<f6>" #'markdown-toggle-inline-images)
(keymap-set markdown-mode-map "C-/" #'cc/emphasize-dwim)
;; (define-key markdown-mode-map [f13] 'markdown-preview)

(add-hook 'markdown-mode-hook #'cc/save-hook-delete-trailing-whitespace)
(add-hook 'markdown-mode-hook #'imenu-add-menubar-index)
(add-hook 'markdown-mode-hook (lambda () (setq-local imenu-auto-rescan t)))

(transient-define-prefix cc/markdown-mode-tmenu ()
  ["Markdown"
   ["Edit"
    ("is" "Insert Source Cookie…" cc/markdown-insert-src-cookie)]

   ["Convert"
    ("mh" "MD Image to HTML" cc/convert-md-image-to-html
     :inapt-if-not use-region-p)

    ;; ("" "" cc/markdown-to-org-region)
    ]

   ["Pelican"
    :pad-keys t
    ("pt" "Insert Timestamp" cc/pelican-timestamp)
    ("fi" "Fix image refs" cc/pelican-fix-image-src-refs
     :inapt-if-not use-region-p)
    ("S" "Start Server…" cc/devserver)]

   ["Display"
    ("h" "Hide Markup" markdown-toggle-markup-hiding
     :description (lambda ()
                    (casual-lib-checkbox-label
                     markdown-hide-markup
                     "Hide Markup"))
     :transient t)
    ("P" "Toggle Prettify" prettify-symbols-mode
     :description (lambda () (casual-lib-checkbox-label prettify-symbols-mode
                                                   "Prettify")))]]

  [:class transient-row
   (casual-lib-quit-one)
   (casual-lib-quit-all)])

(keymap-set markdown-mode-map "M-m" #'cc/markdown-mode-tmenu)

(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'ediff-prepare-buffer-hook #'outline-show-all 0 t)))

(provide 'cc-markdown-mode)
;;; cc-markdown-mode.el ends here
