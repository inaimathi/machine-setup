(require 'cl-lib)
(require 'color)

;;; lisp basics
(defun flash-region (start end &optional timeout)
  "Temporarily highlight region from START to END. Ripped bleeding from SLIME."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

(defun flash-defun-at-point ()
  (interactive)
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
	  (end (progn (end-of-defun) (point))))
      (flash-region start end))))

(defun flash-sexp-at-point ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'sexp))
	 (start (car bounds))
	 (end (cdr bounds)))
    (flash-region start end)))

(defun macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (flash-sexp-at-point)
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defmacro -if-let (name test then &optional else)
  (let ((tmp (gensym)))
    `(let ((,tmp ,test))
       (if ,tmp
	   (let ((,name ,tmp)) ,then)
	 ,else))))

;;; Dealing with directories
(defun starts-with-dot-p (path)
  (= (aref path 0) ?.))

(defun list-subdirectories (path)
  (let ((all (mapcar
	      (lambda (name) (concat (file-name-as-directory path) name))
	      (cl-remove-if #'starts-with-dot-p (directory-files path)))))
    (cl-remove-if-not #'file-directory-p all)))

(defun add-to-load-path (dirs)
  (mapc (lambda (p) (add-to-list 'load-path p)) dirs))

;;; key and mode declaration shortcuts
(defmacro def-sparse-map (name/doc &rest key/fn-list)
  (cl-assert (and (listp name/doc)
	       (symbolp (car name/doc))
	       (or (not (cadr name/doc))
		   (stringp (cadr name/doc)))))
  `(defvar ,(car name/doc)
     (keys (make-sparse-keymap) ,@key/fn-list)
     ,@(cdr name/doc)))

(defmacro keys (keymap &rest key/fn-list)
  `(let ((map ,keymap))
     ,@(cl-loop for (key fn) on key/fn-list by #'cddr
	     collect `(define-key map (kbd ,key) ,fn))
     map))

(defmacro hooks (mode-name/s &rest functions)
  (cl-assert (or (symbolp mode-name/s) (listp mode-name/s)))
  (cl-flet ((to-hook (name) (intern (format "%s-mode-hook" name))))
    (cond
     ((and (symbolp mode-name/s) (not (cdr functions)))
      `(add-hook ',(to-hook mode-name/s) ,(car functions)))
     ((symbolp mode-name/s)
      `(progn
	 ,@(cl-loop for f in functions
		 collect `(add-hook ',(to-hook mode-name/s) ,f))))
     ((not (cdr functions))
      (let ((fn (gensym)))
	`(let ((,fn ,(car functions)))
	   ,@(cl-loop for m in mode-name/s
		   collect `(add-hook ',(to-hook m) ,fn)))))
     (t
      (let ((f-bindings (cl-loop for f in functions collect (list (gensym) f))))
	`(let ,f-bindings
	   ,@(cl-loop for m in mode-name/s
		   append (cl-loop for b in f-bindings
				collect `(add-hook ',(to-hook m) ,(car b)) ))))))))

(defmacro by-ext (extension/s mode)
  (cl-assert (or (stringp extension/s) (listp extension/s)))
  (cl-flet ((to-reg (ext) (format "\\.%s$" ext)))
    (if (stringp extension/s)
	`(add-to-list 'auto-mode-alist '(,(to-reg extension/s) . ,mode))
      `(progn ,@(cl-loop for ext in extension/s
		      collect `(add-to-list 'auto-mode-alist
					    '(,(to-reg ext) . ,mode)))))))

(defmacro global-mode (mode-name) ;;shortcut for globalizing a minor mode (since I do it more than once)
  (let ((g-name (make-symbol (concat "global-" (symbol-name mode-name)))))
    `(progn
       (define-globalized-minor-mode ,g-name
	   ,mode-name
	 (lambda () (,mode-name t)))
       (,g-name t))))

;;; basic word/char count
(defun word-count () ;; note that Emacs doesn't count hyphenated words as a single word
  (interactive)
  (count-text 'forward-word "words"))

(defun char-count ()
  (interactive)
  (count-text 'forward-char "characters"))

(defun count-text (inc-function items)
  (save-excursion
    (beginning-of-buffer)
    (message "%d %s"
	     (cl-loop for count from 0 do (funcall inc-function) if (eobp) return count)
	     items)))

;;; Other
(defun file-nameify-string (a-string)
  (replace-regexp-in-string
   " +" "-"
   (replace-regexp-in-string
    "[',`~_\"]" ""
    (replace-regexp-in-string
     "part[ 	]\\([0-9]+\\)" "p\\1"
     (downcase a-string)))))

(defun hex->rgb (hex-string)
  (mapcar
   (lambda (h)
     (/ (float (string-to-number h 16)) (float 255)))
   (list
    (substring hex-string 1 3)
    (substring hex-string 3 5)
    (substring hex-string 5))))

(defvar conv-colors
  (list "LightBlue2" "LightCyan"
	"LightGoldenrod" "LightGoldenrod4" "LightGoldenrodYellow"
	"LightGreen" "LightPink1" "LightSalmon" "LightSeaGreen"
	"LightSkyBlue" "LightSlateGray"))

(defcustom
  conv-color-map (list)
  "An alist that stores the mapping of virtual-envs to colors")

(defun virtual-env-name ()
  (-if-let venv (getenv "VIRTUAL_ENV")
    (file-name-nondirectory venv)))

(defun python-version ()
  (mapcar
   #'string-to-number
   (split-string
    (cadr
     (split-string
      (string-trim
       (shell-command-to-string "python --version"))))
    "\\.")))

(defun color-of (input)
  (or
   (cdr (assoc input conv-color-map #'string=))
   (-if-let clr (car (cl-set-difference conv-colors (mapcar #'cdr conv-color-map) :test #'string=))
     (let ((new-colors (cons (cons input clr) conv-color-map)))
       (customize-save-variable 'conv-color-map new-colors)
       clr))))

(defun set-colors-by (input)
  (when (and input (color-of input))
    (set-background-color (color-of input))))

(defun load-dotenv (file)
  (interactive "fPath to .env: ")
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
      (let ((key (match-string 1))
            (value (match-string 2)))
        (setenv key value)))))

(provide 'convenience)
