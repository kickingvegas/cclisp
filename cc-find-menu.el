;; Find Menu

(require 'cc-context-menu-macros)

(defvar cc/find-menu (make-sparse-keymap "Find")
  "Keymap for Find submenu.")

(cc/add-first-context-menu-item cc/find-menu
                          rgrep
                          "Find in files (rgrep)…"
                          "Recursively find for Emacs regex in files rooted at \
current buffer")

(cc/add-context-menu-item cc/find-menu
                          query-replace
                          "Query Replace…"
                          "Replace some occurrences of from-string with \
to-string")

(cc/add-context-menu-item cc/find-menu
                          query-replace-regexp
                          "Query Replace Regex…"
                          "Replace some occurrences of Emacs regex with \
to-string")


(cc/add-context-menu-item cc/find-menu
                          re-builder
                          "Regex Builder…"
                          "Construct an Emacs regex interactively")

(provide 'cc-find-menu)

