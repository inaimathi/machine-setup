(require 'request)

(defun aidev--chat (system messages)
  (string-trim (aidev---ollama messages system)))

(defun aidev---ollama-check-connectivity (url)
  "Check if there's a listening Ollama server at URL."
  (let* ((parsed-url (url-generic-parse-url url))
         (host (url-host parsed-url))
         (port (url-port parsed-url))
         (connected nil))
    (and host port
         (condition-case nil
             (let ((proc (make-network-process
                          :name "ollama-test"
                          :host host
                          :service port
                          :nowait t)))
               (set-process-sentinel
                proc
                (lambda (proc event)
                  (when (string-match "open" event)
                    (setq connected t))))
               (sleep-for 0.2)
               (delete-process proc)
               connected)
           (error nil)))))

(defvar aidev---ollama-default-url
  (let ((env-address (getenv "AIDEV_OLLAMA_ADDRESS")))
    (or env-address
	(and (aidev---ollama-check-connectivity "http://192.168.0.12:11434/")
             "http://192.168.0.12:11434/")
	(and (aidev---ollama-check-connectivity "http://localhost:11434/")
             "http://localhost:11434/"))))

(defun aidev---ollama (messages &optional system model)
  "Send MESSAGES to Ollama API.
MODEL defaults to \"deepseek-coder-v2:latest\".
SYSTEM is an optional system prompt."
  (unless aidev---ollama-default-url
    (signal 'ollama-url-unset nil))
  (let* ((model (or model "deepseek-coder-v2:latest"))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data
          (json-encode
           `((messages . ,(if system
                              (cons `((role . "system")
                                      (content . ,system))
                                    messages)
                            messages))
             (model . ,model))))
         (response-buffer
          (url-retrieve-synchronously
           (concat aidev---ollama-default-url "/api/chat")))
         response)
    (unwind-protect
	(with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (cdr (assoc 'content (cdr (assoc 'message response))))))

(defun aidev---openai (messages &optional system model)
  "Send MESSAGES to OpenAI API.
MODEL defaults to \"gpt-4-0-latest\".
SYSTEM is an optional system prompt."
  (let* ((model (or model "chatgpt-4o-latest"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
         (url-request-data
          (json-encode
           `((messages . ,(if system
                              (cons `((role . "system")
                                      (content . ,system))
                                    messages)
                            messages))
             (model . ,model))))
         (response-buffer
          (url-retrieve-synchronously
           "https://api.openai.com/v1/chat/completions"))
         response)
    (message "GOT RESPONSE")
    (unwind-protect
	(with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (message "RESPONSE: %s" response)
    (cdr (assoc 'content (cdr (assoc 'message (aref (cdr (assoc 'choices response)) 0)))))))

(defun aidev---claude (messages &optional system model)
  "Send MESSAGES to Claude API.
MODEL defaults to \"claude-3-5-sonnet-20240620\".
SYSTEM is an optional system prompt."
  (let* ((model (or model "claude-3-5-sonnet-20240620"))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("X-Api-Key" . ,(getenv "ANTHROPIC_API_KEY"))
            ("anthropic-version" . "2023-06-01")))
         (url-request-data
          (json-encode
           (append
            `((messages . ,messages)
              (model . ,model)
              (max_tokens . 4096))
            (when system
              `((system . ,system))))))
         (response-buffer
          (url-retrieve-synchronously
           "https://api.anthropic.com/v1/messages"))
         response)
    (unwind-protect
	(with-current-buffer response-buffer
          (goto-char (point-min))
          (re-search-forward "^$")
          (forward-char)
          (setq response (json-read)))
      (kill-buffer response-buffer))
    (cdr (assoc 'text (aref (cdr (assoc 'content response)) 0)))))

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
