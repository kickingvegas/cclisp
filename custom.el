(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-additional-directory-list '("/opt/local/share/info"))
 '(Info-scroll-prefer-subnodes nil)
 '(Man-notify-method 'aggressive)
 '(apropos-do-all t)
 '(auto-insert-alist
   '((("\\.\\([Hh]\\|hh\\|hpp\\|hxx\\|h\\+\\+\\)\\'" . "C / C++ header")
      (replace-regexp-in-string "[^A-Z0-9]" "_"
                                (string-replace "+" "P"
                                                (upcase
                                                 (file-name-nondirectory buffer-file-name))))
      "#ifndef " str n "#define " str "\12\12" _ "\12\12#endif")
     (("\\.\\([Cc]\\|cc\\|cpp\\|cxx\\|c\\+\\+\\)\\'" . "C / C++ program")
      nil "#include \""
      (let
          ((stem
            (file-name-sans-extension buffer-file-name))
           ret)
        (dolist
            (ext
             '("H" "h" "hh" "hpp" "hxx" "h++")
             ret)
          (when
              (file-exists-p
               (concat stem "." ext))
            (setq ret
                  (file-name-nondirectory
                   (concat stem "." ext))))))
      & 34 | -10)
     (("[Mm]akefile\\'" . "Makefile")
      . "makefile.inc")
     (html-mode lambda nil
                (sgml-tag "html"))
     (plain-tex-mode . "tex-insert.tex")
     (bibtex-mode . "tex-insert.tex")
     (latex-mode "options, RET: " "\\documentclass[" str & 93 | -1 123
                 (read-string "class: ")
                 "}\12"
                 ("package, %s: " "\\usepackage["
                  (read-string "options, RET: ")
                  & 93 | -1 123 str "}\12")
                 _ "\12\\begin{document}\12" _ "\12\\end{document}")
     (("/bin/.*[^/]\\'" . "Shell-Script mode magic number")
      lambda nil
      (if
          (eq major-mode
              (default-value 'major-mode))
          (sh-mode)))
     (ada-mode . ada-header)
     (("\\.[1-9]\\'" . "Man page skeleton")
      "Short description: " ".\\\" Copyright (C), "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "\12.\\\" You may distribute this file under the terms of the GNU Free\12.\\\" Documentation License.\12.TH "
      (file-name-base
       (buffer-file-name))
      " "
      (file-name-extension
       (buffer-file-name))
      " "
      (format-time-string "%Y-%m-%d ")
      "\12.SH NAME\12"
      (file-name-base
       (buffer-file-name))
      " \\- " str "\12.SH SYNOPSIS\12.B "
      (file-name-base
       (buffer-file-name))
      "\12" _ "\12.SH DESCRIPTION\12.SH OPTIONS\12.SH FILES\12.SH \"SEE ALSO\"\12.SH BUGS\12.SH AUTHOR\12"
      (user-full-name)
      '(if
           (search-backward "&"
                            (line-beginning-position)
                            t)
           (replace-match
            (capitalize
             (user-login-name))
            t t))
      '(end-of-line 1)
      " <"
      (progn user-mail-address)
      ">\12")
     (".dir-locals.el" nil ";;; Directory Local Variables\12" ";;; For more information see (info \"(emacs) Directory Variables\")\12\12" "(("
      '(setq v1
             (let
                 (modes)
               (mapatoms
                (lambda
                  (mode)
                  (let
                      ((name
                        (symbol-name mode)))
                    (when
                        (string-match "-mode$" name)
                      (push name modes)))))
               (sort modes 'string<)))
      (completing-read "Local variables for mode: " v1 nil t)
      " . (("
      (let
          ((all-variables
            (apropos-internal ".*"
                              (lambda
                                (symbol)
                                (and
                                 (boundp symbol)
                                 (get symbol 'variable-documentation))))))
        (completing-read "Variable to set: " all-variables))
      " . "
      (completing-read "Value to set it to: " nil)
      "))))\12")
     (("\\.el\\'" . "Emacs Lisp header")
      "Short description: " ";;; "
      (file-name-nondirectory
       (buffer-file-name))
      " --- " str
      (make-string
       (max 2
            (- 80
               (current-column)
               27))
       32)
      "-*- lexical-binding: t; -*-"
      '(setq lexical-binding t)
      "\12\12;; Copyright (C) "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "\12\12;; Author: "
      (user-full-name)
      '(if
           (search-backward "&"
                            (line-beginning-position)
                            t)
           (replace-match
            (capitalize
             (user-login-name))
            t t))
      '(end-of-line 1)
      " <"
      (progn user-mail-address)
      ">\12;; Keywords: "
      '(require 'finder)
      '(setq v1
             (mapcar
              (lambda
                (x)
                (list
                 (symbol-name
                  (car x))))
              finder-known-keywords)
             v2
             (mapconcat
              (lambda
                (x)
                (format "%12s:  %s"
                        (car x)
                        (cdr x)))
              finder-known-keywords "\12"))
      ((let
           ((minibuffer-help-form v2))
         (completing-read "Keyword, C-h: " v1 nil t))
       str ", ")
      & -2 "\12\12;; This program is free software; you can redistribute it and/or modify\12;; it under the terms of the GNU General Public License as published by\12;; the Free Software Foundation, either version 3 of the License, or\12;; (at your option) any later version.\12\12;; This program is distributed in the hope that it will be useful,\12;; but WITHOUT ANY WARRANTY; without even the implied warranty of\12;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\12;; GNU General Public License for more details.\12\12;; You should have received a copy of the GNU General Public License\12;; along with this program.  If not, see <https://www.gnu.org/licenses/>.\12\12;;; Commentary:\12\12;; " _ "\12\12;;; Code:\12\12\12\12(provide '"
      (file-name-base
       (buffer-file-name))
      ")\12;;; "
      (file-name-nondirectory
       (buffer-file-name))
      " ends here\12")
     (("\\.texi\\(nfo\\)?\\'" . "Texinfo file skeleton")
      "Title: " "\\input texinfo   @c -*-texinfo-*-\12@c %**start of header\12@setfilename "
      (file-name-base
       (buffer-file-name))
      ".info\12" "@settitle " str "\12@c %**end of header\12@copying\12"
      (setq short-description
            (read-string "Short description: "))
      ".\12\12" "Copyright @copyright{} "
      (format-time-string "%Y")
      "  "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      "\12\12@quotation\12Permission is granted to copy, distribute and/or modify this document\12under the terms of the GNU Free Documentation License, Version 1.3\12or any later version published by the Free Software Foundation;\12with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.\12A copy of the license is included in the section entitled ``GNU\12Free Documentation License''.\12\12A copy of the license is also available from the Free Software\12Foundation Web site at @url{https://www.gnu.org/licenses/fdl.html}.\12\12@end quotation\12\12The document was typeset with\12@uref{https://www.gnu.org/software/texinfo/, GNU Texinfo}.\12\12@end copying\12\12@titlepage\12@title " str "\12@subtitle " short-description "\12@author "
      (getenv "ORGANIZATION")
      |
      (progn user-full-name)
      " <"
      (progn user-mail-address)
      ">\12@page\12@vskip 0pt plus 1filll\12@insertcopying\12@end titlepage\12\12@c Output the table of the contents at the beginning.\12@contents\12\12@ifnottex\12@node Top\12@top " str "\12\12@insertcopying\12@end ifnottex\12\12@c Generate the nodes for this menu with `C-c C-u C-m'.\12@menu\12@end menu\12\12@c Update all node entries with `C-c C-u C-n'.\12@c Insert new nodes with `C-c C-c n'.\12@node Chapter One\12@chapter Chapter One\12\12" _ "\12\12@node Copying This Manual\12@appendix Copying This Manual\12\12@menu\12* GNU Free Documentation License::  License for copying this manual.\12@end menu\12\12@c Get fdl.texi from https://www.gnu.org/licenses/fdl.html\12@include fdl.texi\12\12@node Index\12@unnumbered Index\12\12@printindex cp\12\12@bye\12")
     (("\\.py\\'" . "Python")
      . "python3_script_skeleton_2024.py")
     (("\\.sh\\'" . "Shell Script")
      . "bash-template.sh")))
 '(auto-insert-directory "~/Templates")
 '(auto-insert-mode t)
 '(bibtex-completion-bibliography '("~/org/bib/references.bib"))
 '(bookmark-save-flag 1)
 '(calc-kill-line-numbering nil)
 '(calendar-latitude 37.7643)
 '(calendar-longitude -122.4753)
 '(case-fold-search t)
 '(casual-dired-use-unicode-symbols t)
 '(casual-dired-use-utf8-symbols t)
 '(cc-dired-listing-switches
   '("--human-readable" "--group-directories-first" "--time-style=long-iso"))
 '(column-number-mode t)
 '(current-language-environment "English")
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(dabbrev-case-fold-search 'case-fold-search)
 '(dabbrev-upcase-means-case-search t)
 '(delete-by-moving-to-trash t)
 '(delete-selection-mode t)
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(dired-guess-shell-alist-user '(("" "open")))
 '(dired-listing-switches
   "-lh --group-directories-first --time-style=long-iso -g --no-group")
 '(dired-mouse-drag-files t)
 '(dired-use-ls-dired t)
 '(display-buffer-alist
   '(("\\*eshell\\*"
      (display-buffer-at-bottom)
      (window-height . 0.23)
      (dedicated . t))
     ("\\*Occur\\*"
      (display-buffer-reuse-window display-buffer-below-selected)
      (dedicated . t)
      (body-function . select-window))
     ("\\*grep\\*" nil
      (body-function . select-window))
     ("\\*shell\\*"
      (display-buffer-reuse-window display-buffer-below-selected))))
 '(display-time-day-and-date t)
 '(display-time-mode t)
 '(ebib-bibtex-dialect 'biblatex)
 '(ebib-default-directory "~/org/bib")
 '(ebib-preload-bib-files '("~/org/bib/references.bib"))
 '(ediff-keep-variants nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elfeed-feeds
   '(("https://developer.apple.com/news/site-updates/rss/site-updates.rss" apple)
     ("https://developer.apple.com/news/rss/news.rss" apple)
     ("https://developer.apple.com/news/releases/rss/releases.rss" apple)
     ("https://news.ycombinator.com/rss" tech hn)
     ("http://yummymelon.com/devnull/feeds/all.atom.xml" devnull)
     ("https://daringfireball.net/feeds/main" tech df)
     ("https://www.theverge.com/rss/index.xml" tech verge)
     ("https://rss.nytimes.com/services/xml/rss/nyt/HomePage.xml" nyt)
     ("https://feeds.arstechnica.com/arstechnica/index" tech ars)
     ("http://feeds.washingtonpost.com/rss/national?itid=lk_inline_manual_32" wapo national)
     ("http://feeds.washingtonpost.com/rss/world?itid=lk_inline_manual_36" wapo world)
     ("http://feeds.washingtonpost.com/rss/business?itid=lk_inline_manual_37" wapo business)
     ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" world nyt)
     ("https://rss.nytimes.com/services/xml/rss/nyt/Arts.xml" arts nyt)
     ("https://rss.nytimes.com/services/xml/rss/nyt/FashionandStyle.xml" style nyt)
     ("https://rss.nytimes.com/services/xml/rss/nyt/Health.xml" health nyt)
     ("https://rss.nytimes.com/services/xml/rss/nyt/Sports.xml" nyt sports)
     ("https://rss.nytimes.com/services/xml/rss/nyt/Technology.xml" tech nyt)
     ("https://rss.nytimes.com/services/xml/rss/nyt/Business.xml" business nyt)
     ("https://irreal.org/blog/?feed=rss2" tech emacs)
     ("https://9to5mac.com/rss" apple tech)
     ("https://spectrum.ieee.org/rss" spectrum tech)
     ("https://feeds.macrumors.com/MacRumors-All" apple tech)
     ("https://sachachua.com/blog/feed" tech emacs)
     ("https://erinkissane.com/feed.rss" opinion tech)
     ("https://www.avclub.com/rss" media)
     ("https://sf.eater.com/rss/index.xml" sf food)
     ("https://missionlocal.org/feed" sf)
     ("https://sfstandard.com/feed/" sf)))
 '(eshell-modules-list
   '(eshell-alias eshell-banner eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-term eshell-unix))
 '(eshell-scroll-to-bottom-on-input 'this)
 '(eshell-scroll-to-bottom-on-output 'this)
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-mark-ring-max 8)
 '(graphviz-dot-indent-width 4)
 '(graphviz-dot-preview-extension "svg")
 '(hi-lock-auto-select-face t)
 '(highlight-nonselected-windows nil)
 '(hippie-expand-try-functions-list
   '(try-expand-dabbrev try-expand-line try-expand-list try-expand-dabbrev-all-buffers try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(ignored-local-variable-values
   '((vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(isearch-lazy-count t)
 '(lazy-count-prefix-format nil)
 '(lazy-count-suffix-format " [%s of %s]")
 '(locate-command "mdfind")
 '(magit-save-repository-buffers 'dontask)
 '(mark-ring-max 6)
 '(markdown-command "multimarkdown")
 '(markdown-header-scaling nil)
 '(org-agenda-files '("~/org/"))
 '(org-agenda-include-diary t)
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-up)
     (todo todo-state-down priority-down timestamp-down category-up)
     (tags todo-state-down priority-down timestamp-down category-up)
     (search todo-state-down priority-down timestamp-down time-up category-up)))
 '(org-agenda-start-with-log-mode '(closed clock))
 '(org-agenda-window-setup 'other-window)
 '(org-babel-python-command "python3")
 '(org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))
 '(org-clock-persist t)
 '(org-clock-sound "/System/Library/Sounds/Glass.aiff")
 '(org-confirm-babel-evaluate nil)
 '(org-display-remote-inline-images 'cache)
 '(org-export-allow-bind-keywords t)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-export-with-smart-quotes t)
 '(org-export-with-sub-superscripts '{})
 '(org-export-with-toc nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-hide-emphasis-markers t)
 '(org-hide-leading-stars t)
 '(org-html-postamble nil)
 '(org-icalendar-include-todo t)
 '(org-icalendar-use-scheduled '(event-if-todo event-if-todo-not-done todo-start))
 '(org-insert-heading-respect-content t)
 '(org-latex-classes
   '(("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("simpleresumecv" "\\documentclass[11pt]{simpleresumecv}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
 '(org-latex-compiler "xelatex")
 '(org-latex-pdf-process
   '("%latex -interaction nonstopmode --shell-escape -output-directory %o %f" "%latex -interaction nonstopmode --shell-escape -output-directory %o %f" "%latex -interaction nonstopmode --shell-escape -output-directory %o %f"))
 '(org-latex-prefer-user-labels t)
 '(org-plantuml-jar-path "/opt/local/share/java/plantuml/plantuml.jar")
 '(org-re-reveal-theme "moon")
 '(org-show-notification-handler 'cc/display-notification)
 '(org-src-lang-modes
   '(("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . graphviz-dot)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("shell" . sh)
     ("bash" . sh)
     ("plantuml" . plantuml)
     ("swift" . swift)
     ("swiftui" . swift)
     ("graphviz" . graphviz)
     ("mscgen" . mscgen)))
 '(org-startup-folded 'showeverything)
 '(org-startup-with-inline-images t)
 '(org-superstar-headline-bullets-list '(10687 10070 10040 10047))
 '(org-superstar-leading-bullet 32)
 '(org-support-shift-select t)
 '(org-use-speed-commands t)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "http://melpa.org/packages/")))
 '(package-selected-packages
   '(casual-dired casual cc-isearch-menu org-ql flycheck-package package-lint iedit doct scpaste snow paredit orgtbl-aggregate transpose-frame diff-hl keycast edit-indirect ox-trac google-translate org wgrep js2-mode rainbow-mode use-package bind-key reveal-in-folder elfeed ob-swiftui ob-swift csv-mode company-restclient visual-regexp-steroids visual-regexp citar math-symbol-lists helm-bibtex ox-gist org-ref org-re-reveal webpaste company-org-block company eglot gnuplot ob-restclient restclient ox-slack good-scroll svg-clock disk-usage expand-region helm-pass password-store which-key org-outline-numbering org-superstar osx-dictionary spotlight ebib auto-complete plantuml-mode tj3-mode ledger-mode yasnippet-snippets yasnippet htmlize calfw kanban fireplace treemacs neotree smart-mode-line-powerline-theme pbcopy ox-jira ox-gfm helm-swoop helm ztree yaml-mode swift-mode sr-speedbar solarized-theme python-mode pkg-info markdown-mode magit json-mode graphviz-dot-mode google-this go-mode autopair))
 '(plantuml-default-exec-mode 'executable)
 '(plantuml-executable-path "/opt/local/bin/plantuml")
 '(plantuml-indent-level 4)
 '(py-shell-name "python3" t)
 '(python-shell-interpreter "python3")
 '(reb-re-syntax 'string)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((eval and buffer-file-name
           (not
            (eq major-mode 'package-recipe-mode))
           (or
            (require 'package-recipe-mode nil t)
            (let
                ((load-path
                  (cons "../package-build" load-path)))
              (require 'package-recipe-mode nil t)))
           (package-recipe-mode))
     (vc-git-annotate-switches . "-w")))
 '(savehist-mode t)
 '(scroll-bar-mode nil)
 '(scroll-step 4)
 '(sentence-end-double-space nil)
 '(set-mark-command-repeat-pop t)
 '(speedbar-show-unknown-files t)
 '(split-width-threshold nil)
 '(switch-to-buffer-obey-display-actions t)
 '(tramp-terminal-type "tramp")
 '(trash-directory "~/.Trash")
 '(use-file-dialog nil)
 '(user-mail-address "kickingvegas@gmail.com")
 '(vc-make-backup-files nil)
 '(warning-suppress-types '((comp)))
 '(wdired-allow-to-change-permissions t)
 '(wgrep-auto-save-buffer t)
 '(world-clock-list
   '(("Pacific/Honolulu" "Honolulu")
     ("America/Los_Angeles" "San Francisco")
     ("America/Denver" "Denver")
     ("America/Chicago" "Chicago")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Berlin" "Barcelona • Paris • Berlin")
     ("Europe/Kiev" "Kyiv")
     ("Asia/Shanghai" "Shanghai")
     ("Asia/Singapore" "Singapore")
     ("Asia/Tokyo" "Tokyo")
     ("Asia/Seoul" "Seoul"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((type mac ns)) (:inherit nil :extend nil :stipple nil :background "mac:textBackgroundColor" :foreground "mac:textColor" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 151 :width normal :foundry "nil" :family "Menlo")) (((type x pgtk)) (:extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :family "Liberation Mono"))))
 '(Man-overstrike ((((class color) (background light)) (:inherit bold :foreground "dark slate blue")) (((class color) (background dark)) (:inherit bold :foreground "gold")) (((type tty) (class color)) (:inherit bold :foreground "gold"))))
 '(Man-underline ((((type x mac ns) (class color) (background light)) (:inherit underline :foreground "magenta")) (((type x mac ns) (class color) (background dark)) (:inherit underline :foreground "lawn green")) (((type tty) (class color)) (:inherit underline :foreground "lawn green"))))
 '(calendar-today ((t (:foreground "magenta" :underline "dark violet" :height 1.0))))
 '(calendar-weekend-header ((t (:inherit font-lock-comment-face))))
 '(comint-highlight-prompt ((((type x mac ns) (class color) (background light)) (:inherit minibuffer-prompt :foreground "dark magenta")) (((type x mac ns) (class color) (background dark)) (:inherit minibuffer-prompt :foreground "tomato")) (((type tty) (class color)) (:foreground "tomato"))))
 '(diff-added ((t (:inherit diff-changed :extend t :background "lawn green"))))
 '(eshell-prompt ((t (:foreground "#cc33ff" :weight bold))))
 '(fixed-pitch ((((type mac ns)) (:height 0.8 :family "Menlo")) (((type x pgtk)) (:height 0.8 :family "Liberation Mono"))))
 '(font-lock-comment-face ((((type x mac ns) (class color) (background light)) (:foreground "dim gray")) (((type x mac ns) (class color) (background dark)) (:foreground "dark gray")) (((type tty) (class color)) (:foreground "medium turquoise"))))
 '(font-lock-constant-face ((t (:height 1.0))))
 '(font-lock-function-name-face ((((type x mac ns) (class color) (background light)) (:foreground "Blue1")) (((type x mac ns) (class color) (background dark)) (:foreground "LightSkyBlue")) (((type tty) (class color)) (:foreground "yellow")) (t (:weight bold))))
 '(highlight ((((type x mac ns) (class color) (background light)) (:background "alice blue")) (((type x mac ns) (class color) (background dark)) (:background "gray20")) (((type tty) (class color)) (:background "gray20")) (t (:inverse-video t))))
 '(hl-line ((t (:inherit highlight :extend t))))
 '(markdown-code-face ((t (:inherit fixed-pitch))))
 '(markdown-header-delimiter-face ((t (:inherit outline-1))))
 '(markdown-header-face ((t (:inherit outline-1))))
 '(markdown-language-keyword-face ((t (:inherit font-lock-type-face))))
 '(markdown-markup-face ((t (:inherit shadow :slant normal :weight normal))))
 '(minibuffer-prompt ((((type x mac ns) (class color) (background light)) (:foreground "dark magenta")) (((type x mac ns) (class color) (background dark)) (:foreground "orange")) (((type tty)) (:foreground "cyan"))))
 '(mode-line ((((type mac ns)) (:background "#ff7700" :foreground "gray20" :box (:line-width (5 . 5) :color "#ff7700" :style flat-button) :height 1.0 :family "SF Compact Rounded")) (((type x pgtk)) (:background "#ff7700" :foreground "gray20" :box (:line-width (2 . 2) :color "#ff7700" :style flat-button) :height 1.0 :family "Gayathri")) (((type tty)) (:background "#ff7700" :foreground "gray20"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "gray" :box (:line-width (5 . 5) :color "gray" :style flat-button)))))
 '(org-agenda-done ((t (:foreground "snow4"))))
 '(org-agenda-structure ((((type x mac ns) (class color) (background light)) (:foreground "Blue1")) (((type x mac ns) (class color) (background dark)) (:foreground "LightSkyBlue")) (((type tty) (class color)) (:foreground "light sky blue")) (t (:weight bold))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((((type x mac ns) (class color) (background light)) (:inherit fixed-pitch :extend t :background "gray90" :foreground "gray50")) (((type x mac ns) (class color) (background dark)) (:inherit fixed-pitch :extend t :background "grey23" :foreground "tomato")) (((type tty)) (:background "grey23" :foreground "tomato"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-code ((((type x mac ns) (class color)) (:inherit fixed-pitch)) (((type tty) (class color)) (:inherit fixed-pitch :background "color-238"))))
 '(org-date ((((type x mac) (class color) (background light)) (:inherit fixed-pitch :foreground "dark violet" :underline t)) (((type mac ns) (class color) (background dark)) (:inherit fixed-pitch :foreground "dark turquoise" :underline t)) (((type tty) (class color)) (:foreground "dark turquoise" :underline t))))
 '(org-document-info ((((class color) (background light)) (:foreground "midnight blue")) (((class color) (background dark)) (:foreground "pale turquoise")) (t nil) (((type tty) (class color)) (:foreground "pale turquoise"))))
 '(org-document-info-keyword ((t (:inherit fixed-pitch))))
 '(org-document-title ((((type x mac ns) (class color) (background light)) (:foreground "midnight blue" :weight bold)) (((type x mac ns) (class color) (background dark)) (:foreground "pale turquoise" :weight bold)) (t (:weight bold)) (((type tty) (class color)) (:foreground "pale turquoise"))))
 '(org-formula ((t (:inherit fixed-pitch :foreground "Firebrick"))))
 '(org-hide ((((type x mac ns) (class color) (background light)) (:foreground "white")) (((type x mac ns) (class color) (background dark)) (:foreground "#171717")) (((type tty) (class color)) (:foreground "#171717"))))
 '(org-meta-line ((((type x mac ns) (class color) (background dark)) (:inherit fixed-pitch :foreground "deep sky blue")) (((type x mac ns) (class color) (background light)) (:inherit fixed-pitch :foreground "grey50")) (((type tty) (class color)) (:inherit fixed-pitch :foreground "deep sky blue"))))
 '(org-scheduled ((t (:foreground "#4455ff"))))
 '(org-scheduled-previously ((((type x mac ns) (class color) (background light)) (:foreground "dark violet" :weight bold)) (((type x mac ns) (class color) (background dark)) (:foreground "orchid")) (((type tty) (class color)) (:foreground "orchid"))))
 '(org-scheduled-today ((t (:foreground "#cc4499"))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :height 0.8))))
 '(org-table ((((type x mac ns) (class color) (background light)) (:inherit fixed-pitch :foreground "Blue1")) (((type x mac ns) (class color) (background dark)) (:inherit fixed-pitch :foreground "chartreuse")) (((type tty) (class color)) (:foreground "chartreuse"))))
 '(outline-1 ((((type mac ns)) (:height 1.1 :family "Futura")) (((type x pgtk)) (:height 1.1 :family "Lato"))))
 '(outline-2 ((t (:inherit outline-1))))
 '(outline-3 ((t (:inherit outline-1))))
 '(outline-4 ((t (:inherit outline-1))))
 '(outline-5 ((t (:inherit outline-1))))
 '(outline-6 ((t (:inherit outline-1))))
 '(outline-7 ((t (:inherit outline-1))))
 '(outline-8 ((t (:inherit outline-1))))
 '(region ((((type x mac ns) (class color) (background dark)) (:extend t :background "light sky blue" :foreground "black")) (((type x mac ns) (class color) (background light)) (:extend t :background "light sky blue")) (((type tty) (class color)) (:extend t :background "light sky blue" :foreground "black"))))
 '(secondary-selection ((((class color) (min-colors 88) (background light)) (:extend t :background "yellow1")) (((class color) (min-colors 88) (background dark)) (:extend t :background "gray24")) (((class color) (min-colors 16) (background light)) (:extend t :background "yellow")) (((class color) (min-colors 16) (background dark)) (:extend t :background "gray24")) (((class color) (min-colors 8)) (:extend t :background "cyan" :foreground "black")) (t (:inverse-video t))))
 '(tab-bar ((t (:inherit variable-pitch :background "grey90" :foreground "black" :height 0.75))))
 '(tab-bar-tab ((t (:inherit tab-bar :box (:line-width 1 :color "dim gray")))))
 '(tab-bar-tab-inactive ((t (:inherit tab-bar-tab :background "gainsboro" :foreground "gray62" :box nil))))
 '(variable-pitch ((((type mac ns)) (:height 1.4 :family "Optima")) (((type x pgtk)) (:height 1.4 :family "Nimbus Roman")))))
