;;; cc-edit-text-menu.el --- Edit Text Menus -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Charles Choi

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
(require 'casual-editkit)

(easy-menu-define cc/transpose-menu nil
  "Keymap for Transpose submenu."
  '("Transpose ⇄"
    :visible (not buffer-read-only)
    :enable (not (use-region-p))
    ["Characters" transpose-chars
     :help "Interchange characters around point, moving forward one character."]

    ["Words" transpose-words
     :help "Interchange words around point, leaving point at end of them."]

    ["Lines" transpose-lines
     :help "Exchange current line and previous line, leaving point after both."]

    ["Sentences" transpose-sentences
     :help "Interchange the current sentence with the next one."]

    ["Paragraphs" transpose-paragraphs
     :help "Interchange the current paragraph with the next one."]

    ["Regions" transpose-regions
     :help "region STARTR1 to ENDR1 with STARTR2 to ENDR2."]

    ["Balanced Expressions (sexps)" transpose-sexps
     :help "Like C-t (‘transpose-chars’), but applies to balanced \
expressions (sexps)."]))

(easy-menu-define cc/move-text-menu nil
  "Keymap for Move Text submenu."
  '("Move Text"
    :visible (not buffer-read-only)
    :enable (not (use-region-p))
    ["Word →" casual-editkit-move-word-forward
     :help "Move word to the right of point forward one word."]

    ["Word ←" casual-editkit-move-word-backward
     :help "Move word to the right of point backward one word."]

    ["Sentence →" casual-editkit-move-sentence-forward
     :help "Move sentence to the right of point forward one sentence."]

    ["Sentence ←" casual-editkit-move-sentence-backward
     :help "Move sentence to the right of point backward one sentence."]

    ["Balanced Expression (sexp) →" casual-editkit-move-sexp-forward
     :help "Move balanced expression (sexp) to the right of point forward \
one sexp."]

    ["Balanced Expression (sexp) ←" casual-editkit-move-sexp-backward
     :help "Move balanced expression (sexp) to the right of point backward \
one sexp."]))

(easy-menu-define cc/delete-space-menu nil
  "Keymap for Deleting Space submenu."
  '("Delete"
    :visible (not buffer-read-only)
    ["Join Line" join-line
     :help "Join this line to previous and fix up \
whitespace at join"]

    ["Just One Space" just-one-space
     :help "Delete all spaces and tabs around point, leaving \
one space."]

    ["Delete Horizontal Space" delete-horizontal-space
     :help "Delete all spaces and tabs around point."]

    ["Delete Blank Lines" delete-blank-lines
     :help "On blank line, delete all surrounding blank lines, \
leaving just one."]

    ["Whitespace Cleanup" whitespace-cleanup
     :help "Cleanup some blank problems in all buffer or at region."]

    ["Delete Trailing Whitespace" delete-trailing-whitespace
     :help "Delete trailing whitespace between START and END."]

    ["Zap up to…" zap-up-to-char
     :help "Kill up to, but not including occurrence of CHAR"]

    ["Zap to…" zap-to-char
     :help "Kill up to and including occurrence of CHAR"]))

(provide 'cc-edit-text-menu)

;;; cc-edit-text-menu.el ends here
