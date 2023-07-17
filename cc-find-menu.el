;; Find Menu

(require 'cc-context-menu-macros)

(easy-menu-define cc/find-menu nil
  "Keymap for Find submenu."
  '("Find and/or Replace"
    ["Find in files (rgrep)…" rgrep
     :help "Recursively find for Emacs regex in files rooted at \
current buffer"]
    
    ["Query Replace…" query-replace
     :help "Replace some occurrences of from-string with \
to-string"]
    
    ["Query Replace Regex…" query-replace-regexp
     :help "Replace some occurrences of Emacs regex with \
to-string"]

    ["Regex Builder…" re-builder
     :help "Construct an Emacs regex interactively"]))

(provide 'cc-find-menu)

