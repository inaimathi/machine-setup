(require 'package)

(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))

(defvar +package-list+
  '(elfeed
    aes
    magit highlight-parentheses flex-autopair auto-complete yasnippet paredit
    markdown-mode haskell-mode clojure-mode cider
    py-isort flymake-python-pyflakes))

(unless (cl-every (lambda (p) (require p nil 'noerror)) +package-list+)

  (package-initialize)

  (setq package-archives
	'(("melpa-stable" . "http://stable.melpa.org/packages/")
	  ("melpa" . "http://melpa.org/packages/")
 	  ;;("marmalade" . "https://marmalade-repo.org/packages/")
 	  ("elpa" . "https://elpa.gnu.org/packages/")
	  ))

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p +package-list+)
    (unless (featurep p)
      (package-install p))))

(provide 'packs)
