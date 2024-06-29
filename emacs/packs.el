(require 'package)

(add-to-load-path (list-subdirectories "~/.emacs.d/elpa"))

(defvar +package-list+
  '(aes request
    magit highlight-parentheses auto-complete yasnippet paredit flex-autopair
    rjsx-mode markdown-mode haskell-mode clojure-mode cider sml-mode
    py-isort flymake-python-pyflakes))

(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(unless (cl-every (lambda (p) (require p nil 'noerror)) +package-list+)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (p +package-list+)
    (unless (featurep p)
      (package-install p))))

(provide 'packs)
