;;; cc-edit-text-menu.el --- Edit Text Menus

;;; Commentary:
;;

(easy-menu-define cc/transpose-menu nil
  "Keymap for Transpose submenu"
  '("Transpose"
    :visible (not buffer-read-only)
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
  "Keymap for Move Text submenu"
  '("Move Text"
    :visible (not buffer-read-only)
    ["Word Forward" cc/move-word-forward
     :help "Move word to the right of point forward one word."]

    ["Word Backward" cc/move-word-backward
     :help "Move word to the right of point backward one word."]

    ["Sentence Forward" cc/move-sentence-forward
     :help "Move sentence to the right of point forward one sentence."]

    ["Sentence Backward" cc/move-sentence-backward
     :help "Move sentence to the right of point backward one sentence."]

    ["Balanced Expression (sexp) Forward" cc/move-sexp-forward
     :help "Move balanced expression (sexp) to the right of point forward \
one sexp."]

    ["Balanced Expression (sexp) Backward" cc/move-sexp-backward
     :help "Move balanced expression (sexp) to the right of point backward \
one sexp."]))

(easy-menu-define cc/delete-space-menu nil
  "Keymap for Deleting Space submenu"
  '("Delete Space"
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
     :help "Delete trailing whitespace between START and END."]))


(provide 'cc-edit-text-menu)

;;; cc-edit-text-menu.el ends here
