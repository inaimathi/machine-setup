#!emacs --script

(require 'cl)

(loop for pkg in
      '(elfeed
	aes
        auto-complete yasnippet redo+
	autopair highlight-parentheses paredit
	markdown-mode yaml-mode smart-tab)
      do (package-install pkg))
