(require 'request)

(defun aidev--chat (system messages)
  (let* ((cmd (format
	       "gpt -s %s %s"
	       (shell-quote-argument system)
	       (string-join
		(mapcar
		 (lambda (m) (shell-quote-argument (json-encode m)))
		 messages)
		" ")))
         (result (shell-command-to-string cmd)))
    (string-trim
     (replace-regexp-in-string
      "\\(?:^```[a-zA-Z-]*\\s-*\n\\|\\n?```\\s-*$\\)"
      ""
      result))))

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

(provide 'aidev)
