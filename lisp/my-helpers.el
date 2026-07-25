;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun kzar/describe-eol ()
  (interactive)
  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
    (when (vectorp eol-type)
      (setq eol-type (coding-system-eol-type (aref eol-type 0))))
    (message "Line endings are of type: %s"
             (cl-case eol-type
               (0 "Unix") (1 "DOS") (2 "Mac") (t "Unknown")))))

; http://lists.gnu.org/archive/html/help-gnu-emacs/2009-10/msg00187.html
(defun kzar/sort-csv ()
  (interactive)
  (sort-regexp-fields nil "[^ ,]+" "\\&" (region-beginning) (region-end)))

(defun kzar/indent-rectangle ()
  "Manually indent a region of code, taking care of trailing whitespace."
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (min (region-beginning) (region-end)))
            (end (max (region-beginning) (region-end)))
            (region-indent nil)
            (first-line-start 0)
            (last-line-start 0)
            (last-line-end 0))
        ; Record the start / end positions
        (goto-char end)
        (setq last-line-end (line-end-position))
        (goto-char start)
        (setq first-line-start (line-beginning-position))
        ; Figure out the base indentation for the selected region
        (while (< (point) last-line-end)
          (unless (= (line-end-position) (line-beginning-position))
            (setq region-indent (min (or region-indent (current-indentation))
                                     (current-indentation)))
            (setq last-line-start (line-beginning-position)))
          (forward-line))
        ; Use rectangle mark mode to select the base indentation and
        ; prompt the user to alter that region
        (push-mark first-line-start)
        (goto-char (+ last-line-start region-indent))
        (call-interactively 'string-rectangle)
        ; Clear any trailing whitespace
        (delete-trailing-whitespace (region-beginning) (region-end)))
      (pop-mark))))

(defun kzar/setup-linux-fonts ()
  "Use Symbola for Unicode glyphs and set the default font height."
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend))
  (set-face-attribute 'default nil :height 110))

(provide 'my-helpers)
;;; my-helpers.el ends here
