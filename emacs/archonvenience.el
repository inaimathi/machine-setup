;;; archonvenience.el --- Convenience functions for Zip-Archive/arc-mode -*- lexical-binding: nil -*-

(require 'cl-lib)
(require 'arc-mode)
(require 'seq)

;;;;;;;;;; Archives

(defvar zip-archive-root-whitelist '("src" "tests" "test" "docs" "lib" "include" "scripts" "bin")
  "Top-level path components that `zip-archive--strip-root' will
never strip. If a member's first path segment is one of these,
the full relative path is preserved as-is (no wrapper directory
is assumed). Otherwise, if the member's path has more than one
component, the first component is assumed to be an archive
wrapper directory and is discarded.")

(defun zip-archive--line-int-file ()
  "Return the archive-internal filename shown on the current
Zip-Archive listing line, or nil if this line doesn't look like
a file entry."
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (when (string-match
           "[0-9][0-9]-[A-Za-z][A-Za-z][A-Za-z]-[0-9][0-9][0-9][0-9] +[0-9:]+  \\(.+\\)$"
           line)
      (match-string 1 line))))

(defun zip-archive--strip-root (int-file)
  "Return INT-FILE's path relative to the archive's likely project
root. If INT-FILE has no '/' at all, return it unchanged. If its
first path component is in `zip-archive-root-whitelist', return
INT-FILE unchanged (no wrapper directory assumed). Otherwise,
strip the first path component (treated as an archive wrapper
directory) and return the rest."
  (let ((slash (string-match "/" int-file)))
    (if (not slash)
        int-file
      (let ((first-component (substring int-file 0 slash)))
        (if (member first-component zip-archive-root-whitelist)
            int-file
          (if (string-match "\\`[^/]+/\\(.+\\)\\'" int-file)
              (match-string 1 int-file)
            int-file))))))

(defun zip-archive--path-components (path)
  "Split PATH into a list of non-empty path component strings,
ignoring any leading or trailing slash."
  (delete "" (split-string (directory-file-name path) "/")))

(defun zip-archive--overlap-trim (rel dest-dir)
  "If the trailing directory components of DEST-DIR overlap with
the leading directory components of REL's directory part, drop
that overlapping prefix from REL. This lets you point the expand
destination directly at a subdirectory that's already implied by
a member's path, without duplicating that subdirectory.

For example, if DEST-DIR is \"~/foo/src/pytrivialsql/\" and REL is
\"src/pytrivialsql/mysql.py\", return \"mysql.py\". Bare filenames
\(no directory component) are returned unchanged."
  (let ((rel-dir (file-name-directory rel)))
    (if (not rel-dir)
        rel
      (let* ((filename (file-name-nondirectory rel))
             (rel-components (zip-archive--path-components rel-dir))
             (dest-components (zip-archive--path-components dest-dir))
             (max-k (min (length rel-components) (length dest-components)))
             (best-k 0)
             (k max-k))
        (while (and (> k 0) (= best-k 0))
          (when (equal (last dest-components k) (seq-take rel-components k))
            (setq best-k k))
          (setq k (1- k)))
        (if (= best-k 0)
            rel
          (mapconcat #'identity
                     (append (seq-drop rel-components best-k) (list filename))
                     "/"))))))

(defun zip-archive--expand (dest-dir)
  "Copy the marked members of the current archive (or the member at
point, if none are marked) into DEST-DIR.

Each member's destination path is computed in two steps:
1. `zip-archive--strip-root' discards the archive's own top-level
   wrapper directory (e.g. a GitHub export folder), unless the
   member's path already starts with a recognized project-root
   name (see `zip-archive-root-whitelist') or has no directory
   component at all.
2. `zip-archive--overlap-trim' then drops any leading portion of
   that path which DEST-DIR's trailing components already supply,
   so pointing DEST-DIR straight at a subdirectory a member's path
   implies doesn't duplicate that subdirectory.

Subdirectories are created under DEST-DIR as needed. Existing
files at the destination are overwritten silently. Requires the
`unzip' command-line tool."
  (interactive
   (list (read-directory-name "Expand marked files to: " default-directory)))
  (unless (derived-mode-p 'archive-mode)
    (user-error "Not in an Archive-mode buffer"))
  (let ((archive-file (or (buffer-file-name)
                          (user-error "Buffer is not visiting a file"))))
    (setq dest-dir (file-name-as-directory (expand-file-name dest-dir)))
    (unless (file-directory-p dest-dir)
      (if (y-or-n-p (format "%s does not exist, create it? " dest-dir))
          (make-directory dest-dir t)
        (user-error "Aborted")))
    (let ((mark-char (if (boundp 'archive-mark-char) archive-mark-char ?*))
          (lines '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (eq (char-after) mark-char)
            (push (line-number-at-pos) lines))
          (forward-line 1)))
      (setq lines (nreverse lines))
      (unless lines
        (setq lines (list (line-number-at-pos))))
      (let ((count 0)
            (coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (dolist (ln lines)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- ln))
            (let ((int-file (zip-archive--line-int-file)))
              (when (and int-file (not (string-suffix-p "/" int-file)))
                (let* ((rel (zip-archive--overlap-trim
                             (zip-archive--strip-root int-file) dest-dir))
                       (target (expand-file-name rel dest-dir))
                       (target-dir (file-name-directory target)))
                  (when (and target-dir (not (file-directory-p target-dir)))
                    (make-directory target-dir t))
                  (with-temp-buffer
                    (set-buffer-multibyte nil)
                    (let ((exit (call-process "unzip" nil t nil
                                              "-p" archive-file int-file)))
                      (if (zerop exit)
                          (write-region (point-min) (point-max) target nil 'quiet)
                        (message "Failed to extract %s (exit %d)" int-file exit))))
                  (setq count (1+ count)))))))
        (message "Expanded %d file(s) into %s" count dest-dir)))))

(add-hook 'archive-mode-hook #'hl-line-mode)

(with-eval-after-load 'arc-mode
  (define-key archive-mode-map (kbd "C-c C-e") #'zip-archive--expand))

(provide 'archonvenience)
;;; archonvenience.el ends here
