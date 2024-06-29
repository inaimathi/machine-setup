(require 'request)

(defun aidev--chat (messages)
  (let ((cmd (format
	      "gpt %s"
	      (string-join
	       (mapcar
		(lambda (m) (shell-quote-argument (json-encode m)))
		messages)
	       " "))))
    (string-trim (shell-command-to-string cmd))))

(defun aidev-document-python-region ()
  (interactive)
  (let* ((prompt
	  `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
	    (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
	    (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))
	    (("role" . "user") ("content" . "Write the docstring the above function. Return only the docstring and no other commentary."))))
	 (response (aidev--chat prompt)))
    (goto-char (region-beginning))
    (end-of-line)
    (newline)
    (insert response)))

(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (let ((prompt
	 `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
	   (("role" . "system") ("content" . "The likeliest requests involve generating code. If you are asked to generate code, only return code, and no commentary. If you must, provide minor points and/or testing examples in the form of code comments (commented in the appropriate syntax) but no longer prose unless explicitly requested."))
	   (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
	   ,@(when (region-active-p)
	       `((("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))
	   (("role" . "user") ("content" . ,prompt)))))
    (insert (aidev--chat prompt))))

(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (let ((data (aidev--chat
		 `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
		   (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
		   (("role" . "system") ("content" . "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with."))
		   (("role" . "user") ("content" . ,prompt))
		   (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))))
	  (reg-start (region-beginning))
	  (reg-end (region-end)))
      (goto-char reg-start)
      (delete-region reg-start reg-end)
      (insert data))))

(defun aidev-explain-region ()
  (interactive)
  (insert
   (aidev--chat
    `((("role" . "system")
       ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with. Read over the code and provide the clearest explanation of what the code does, how to use it, and the natural ways in which it might be changed. Return the best answer you possibly can after thinking about it carefully."))
      (("role" . "system")
       ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
      (("role" . "user")
       ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))))

(defun aidev-explain-region-in-particular (prompt)
  (interactive "sPrompt: ")
  (insert
   (aidev--chat
    `((("role" . "system")
       ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with, but they have specific questions. Read over the code and provide the clearest explanation of what the code does, making sure to answer the users' specific question. Return the best answer you possibly can after thinking about it carefully."))
      (("role" . "system")
       ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
      (("role" . "user")
       ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end))))))))

(provide 'aidev)
