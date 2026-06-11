;;; cc-swift-mode.el --- Swift mode configuration    -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Charles Choi

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

(require 'swift-mode)
(require 'swift-ts-mode)
(require 'cclisp)
(require 'eglot)

(keymap-set swift-ts-mode-map "M-[" #'backward-sexp)
(keymap-set swift-ts-mode-map "M-]" #'forward-sexp)
(keymap-set swift-ts-mode-map "M-j" #'fill-paragraph)
(keymap-set swift-ts-mode-map "M-n" #'cc/next-sexp)
(keymap-set swift-ts-mode-map "M-p" #'backward-sexp)
(keymap-set swift-ts-mode-map "C-<up>" #'backward-up-list)
(keymap-set swift-ts-mode-map "C-<down>" #'down-list)
(keymap-set swift-ts-mode-map "C-<left>" #'backward-sexp)
(keymap-set swift-ts-mode-map "C-<right>" #'cc/next-sexp)

(keymap-set swift-ts-mode-map "M-b" #'backward-sexp)
(keymap-set swift-ts-mode-map "M-f" #'cc/next-sexp)
(keymap-set swift-ts-mode-map "C-M-b" #'backward-word)
(keymap-set swift-ts-mode-map "C-M-f" #'forward-word)

(add-hook 'swift-mode-hook (lambda ()
                             (setq fill-column 120)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(swift-ts-mode . ("xcrun" "sourcekit-lsp"))))

(provide 'cc-swift-mode)
;;; cc-swift-mode.el ends here
