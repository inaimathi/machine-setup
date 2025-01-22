(require 'request)

(defun aidev--invert-markdown-code (md-block &optional comment-start comment-end)
  (let* ((lines (split-string md-block "\n"))
         (in-code-block nil)
         (c-start (or comment-start ";; "))
         (c-end (or comment-end ""))
         result)
    (dolist (line lines)
      (if (string-match-p "^[ \t]*```" line)
          (setq in-code-block (not in-code-block))
        (push (if in-code-block
                  line
                (concat c-start line c-end))
              result)))
    (string-join (nreverse result) "\n")))

(defun aidev--strip-markdown-code (md-block)
  (replace-regexp-in-string
   "\\(?:^```[a-zA-Z-]*\\s-*\n\\|\\n?```\\s-*$\\)"
   ""
   md-block))

(defun aidev--chat (system messages)
  (let* ((cmd (format
	       "claude -s %s %s"
	       (shell-quote-argument system)
	       (string-join
		(mapcar
		 (lambda (m) (shell-quote-argument (json-encode m)))
		 messages)
		" ")))
         (result (shell-command-to-string cmd)))
    (string-trim
     (aidev--strip-markdown-code result))))

(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (let ((system
	 (string-join
	  (list
	   "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
	   "The likeliest requests involve generating code. If you are asked to generate code, only return code, and no commentary. If you must, provide minor points and/or testing examples in the form of code comments (commented in the appropriate syntax) but no longer prose unless explicitly requested."
	   (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode))
	  "\n"))
	(prompt
	 `(,@(when (region-active-p)
	       `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
	    (("role" . "user") ("content" . ,prompt)))))
    (insert (aidev--chat system prompt))))

(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (let* ((system (string-join
		    (list
		     "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
		     (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)
		     "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with.")
		    "\n"))
	   (prompt `((("role" . "user")
		      ("content" . ,prompt))
		     (("role" . "user")
		      ("content" . ,(buffer-substring-no-properties
				     (region-beginning) (region-end))))))
	   (data (aidev--chat system prompt))
	   (reg-start (region-beginning))
	   (reg-end (region-end)))
      (goto-char reg-start)
      (delete-region reg-start reg-end)
      (insert data))))

(defun aidev-refactor-buffer-with-chat (prompt)
  "Refactors the current buffer using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (let* ((system (string-join
                  (list
                   "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
                   (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)
                   "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with.")
                  "\n"))
         (prompt `((("role" . "user")
                    ("content" . ,prompt))
                   (("role" . "user")
                    ("content" . ,(buffer-substring-no-properties
                                   (point-min) (point-max))))))
         (data (aidev--chat system prompt)))
    (delete-region (point-min) (point-max))
    (insert data)))

(defun aidev-new-buffer-from-chat (prompt)
  "Creates a new buffer with the result of a chat request."
  (interactive "sPrompt: ")
  (let* ((system
          (string-join
           (list
            "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."
            "The likeliest requests involve generating code. If you are asked to generate code, only return code, and no commentary. If you must, provide minor points and/or testing examples in the form of code comments (commented in the appropriate syntax) but no longer prose unless explicitly requested."
            (format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode))
           "\n"))
         (messages
          `(,@(when (use-region-p)
                `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
            (("role" . "user") ("content" . ,prompt))))
         (result (aidev--chat system messages))
         (new-buffer (generate-new-buffer "*AI Generated Code*")))
    (with-current-buffer new-buffer
      (insert result)
      (funcall major-mode))
    (switch-to-buffer new-buffer)))

(provide 'aidev)
