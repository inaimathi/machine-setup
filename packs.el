(require 'package)

(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))

(package-initialize)

(setq package-archives
      '(("melpa" . "http://melpa.milkbox.net/packages/")
 	("marmalade" . "http://marmalade-repo.org/packages/")
 	("elpa" . "http://elpa.gnu.org/packages/")))

(defvar +package-list+
  '(elfeed
    aes
    magit highlight-parentheses autopair smart-tab
    auto-complete yasnippet paredit
    markdown-mode haskell-mode clojure-mode cider
    flymake-python-pyflakes))

(unless (every #'package-installed-p +package-list+)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p +package-list+)
    (unless (package-installed-p p)
      (package-install p))))

(provide 'packs)
