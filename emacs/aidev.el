(require 'request)

(defun aidev-first-message-content (response)
  (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0))))))

(defun aidev--chat (messages on-success)
  (request "https://api.openai.com/v1/chat/completions"
    :type "POST"
    :data (json-encode `(("messages" . ,messages) ("model" . "gpt-3.5-turbo")))
    :headers `(("Content-Type" . "application/json")
	       ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY"))))
    :parser 'json-read
    :error (cl-function
	    (lambda (&rest args &key error-thrown &allow-other-keys)
	      (message "Got error: %S" error-thrown)))
    :success on-success))

(defun aidev-insert-chat (prompt)
  (interactive "sPrompt: ")
  (aidev--chat
   `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
     (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
     (("role" . "user") ("content" . ,prompt)))
   (cl-function
    (lambda (&key data &allow-other-keys)
      (insert (aidev-first-message-content data))))))

(defun aidev-refactor-region-with-chat (prompt)
  "Refactors the current region using `aidev--chat` function and a prompt."
  (interactive "sPrompt: ")
  (when (use-region-p)
    (aidev--chat
     `((("role" . "system") ("content" . "You are an extremely competent programmer. You have an encyclopedic understanding, high-level understanding of all programming languages and understand how to write the most understandeable, elegant code in all of them."))
       (("role" . "system") ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
       (("role" . "system") ("content" . "The user wants you to help them refactor a piece of code they've already written. Unless specified by their prompt, you should output code in the same language as the input code. Output absolutely nothing but code; the message you return should be a drop-in replacement for the code the user needs help with."))
       (("role" . "user") ("content" . ,prompt))
       (("role" . "user") ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))
     (cl-function
      (lambda (&key data &allow-other-keys)
	(let ((reg-start (region-beginning))
	      (reg-end (region-end)))
	  (goto-char reg-start)
          (delete-region reg-start reg-end)
          (insert (aidev-first-message-content data))))))))

(defun aidev-explain-region ()
  (interactive)
  (aidev--chat
   `((("role" . "system")
      ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with. Read over the code and provide the clearest explanation of what the code does, how to use it, and the natural ways in which it might be changed. Return the best answer you possibly can after thinking about it carefully."))
     (("role" . "system")
      ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
     (("role" . "user")
      ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))
   (cl-function
    (lambda (&key data &allow-other-keys)
      (insert (aidev-first-message-content data))))))

(defun aidev-explain-region-in-particular (prompt)
  (interactive "sPrompt: ")
  (aidev--chat
   `((("role" . "system")
      ("content" . "You are a brilliant writer and veteran programmer, able to put concepts into a simple and straightforward context undestandeable to any reader. You also have a comprehensive understanding of all programming languages from prominent to obscure. The user is asking you to explain a block of code they are working with, but they have specific questions. Read over the code and provide the clearest explanation of what the code does, making sure to answer the users' specific question. Return the best answer you possibly can after thinking about it carefully."))
     (("role" . "system")
      ("content" . ,(format "The user is currently working in the major mode '%s', so please return code appropriate for that context." major-mode)))
     (("role" . "user")
      ("content" . ,(buffer-substring-no-properties (region-beginning) (region-end)))))
   (cl-function
    (lambda (&key data &allow-other-keys)
      (insert (aidev-first-message-content data))))))

(provide 'aidev)
