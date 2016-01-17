#!emacs --script

(package-initialize)

(require 'package)
(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("elpa" . "http://elpa.gnu.org/packages/")))

(require 'cl)

(loop for pkg in
      '(elfeed aes
        auto-complete yasnippet redo+
	autopair highlight-parentheses paredit
	markdown-mode yaml-mode smart-tab)
      do (package-install pkg))
