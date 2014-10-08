(defun describe-eol ()
  (interactive)
  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
    (when (vectorp eol-type)
      (setq eol-type (coding-system-eol-type (aref eol-type 0))))
    (message "Line endings are of type: %s"
             (case eol-type
               (0 "Unix") (1 "DOS") (2 "Mac") (t "Unknown")))))

(defun zap-up-to-char (char)
  (interactive "cCharacter to delete up to: ")
  (zap-to-char 1 char)
  (insert char)
  (backward-char))
