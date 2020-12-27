(require 'package)

(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))

(package-initialize)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
 	;; ("marmalade" . "https://marmalade-repo.org/packages/")
 	("elpa" . "https://elpa.gnu.org/packages/")))

(defvar +package-list+
  '(elfeed
    aes
    magit highlight-parentheses autopair smart-tab
    auto-complete yasnippet paredit
    markdown-mode haskell-mode clojure-mode cider
    py-isort flymake-python-pyflakes))

(unless (cl-every #'featurep +package-list+)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p +package-list+)
    (unless (featurep p)
      (package-install p))))

(provide 'packs)
