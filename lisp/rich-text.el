;;; rich-text.el --- Rich-text conversion to/from the clipboard  -*- lexical-binding: t; -*-

;;; Commentary:
;; Copy Org/Markdown out to the clipboard as rich text
;; (`rich-text/copy-buffer', `rich-text/copy-region') and paste rich text in
;; from the clipboard converted to the buffer's markup (`rich-text/yank').
;;
;; The conversion logic all lives in the companion pandoc module
;; rich-text.lua (kept next to this file), which acts as a custom pandoc
;; reader in the paste direction and as a custom writer in the copy
;; direction.
;;
;; Requires pandoc >= 3.0.

;;; Code:

(defvar rich-text/pandoc-lua
  (expand-file-name "rich-text.lua"
                    (if load-file-name
                        (file-name-directory load-file-name)
                      "~/.emacs.d/"))
  "The pandoc Lua module implementing the rich-text conversions.
Used as a custom pandoc reader when yanking -- it repairs Asana's flat
ProseMirror lists on the raw clipboard HTML, then tidies the parsed document
-- and as a custom pandoc writer when copying -- it wraps links to Asana
objects in Asana's own <span> markup and marks the fragment as editor-native,
so Asana's paste parser keeps links and their text intact.")

;;; The system clipboard, in and out.

(defun rich-text/html-to-clipboard (html)
  "Put the HTML fragment on the system clipboard as text/html, cross-platform.
Web editors like Asana read the HTML flavour on paste; an RTF-only clipboard
they render as nothing, so we set text/html on every platform."
  (cond
   ;; macOS: pbcopy can't set text/html, but osascript can set the HTML flavour
   ;; (public.html) directly, as a hex-encoded data blob.
   ((eq system-type 'darwin)
    (call-process "osascript" nil nil nil
                  "-e" (format "set the clipboard to «data HTML%s»"
                               (mapconcat (lambda (byte) (format "%02x" byte))
                                          (encode-coding-string html 'utf-8) ""))))
   ;; Wayland.
   ((executable-find "wl-copy")
    (with-temp-buffer
      (insert html)
      (call-process-region (point-min) (point-max)
                           "wl-copy" nil nil nil "--type" "text/html")))
   ;; X11.
   ((executable-find "xclip")
    (with-temp-buffer
      (insert html)
      (call-process-region (point-min) (point-max)
                           "xclip" nil nil nil "-selection" "clipboard" "-t" "text/html")))
   (t (user-error "No clipboard backend (osascript/wl-copy/xclip) found"))))

(defun rich-text/clipboard-html ()
  "Return the HTML flavour of the system clipboard as a string, or nil.
Reads the rich-text (text/html) representation the OS keeps alongside plain
text -- what a browser puts there when you copy a formatted selection."
  (let ((html
         (cond
          ;; macOS: the pasteboard exposes HTML as a hex-encoded data blob.
          ((eq system-type 'darwin)
           (shell-command-to-string
            (concat "osascript -e 'the clipboard as «class HTML»' 2>/dev/null"
                    " | sed 's/^«data HTML//; s/»$//' | xxd -r -p")))
          ;; Wayland.
          ((executable-find "wl-paste")
           (shell-command-to-string "wl-paste --type text/html 2>/dev/null"))
          ;; X11.
          ((executable-find "xclip")
           (shell-command-to-string
            "xclip -selection clipboard -t text/html -o 2>/dev/null")))))
    (and html (not (string-empty-p (string-trim html))) html)))

;;; Copy Org/Markdown out to the clipboard as rich text.

(defvar rich-text/pandoc-sources
  '((org-mode      . "org")
    (markdown-mode . "gfm"))
  "Pandoc source format for copying rich text out of each major mode.
Modes are matched with `derived-mode-p', so derived modes (e.g. `gfm-mode')
inherit their parent's entry.  The to-side is always `rich-text/pandoc-lua'.")

(defun rich-text/dedent (text)
  "Strip the common leading indentation from every line of TEXT.
Markdown selected from a nested (indented) position would otherwise be read by
pandoc as an indented code block; removing the shared indent makes it parse at
the top level, while relative nesting inside the selection is preserved."
  (let ((lines (split-string text "\n")) (indent nil))
    (dolist (line lines)
      (unless (string-match-p "\\`[ \t]*\\'" line) ; ignore blank lines
        (let ((n (- (length line) (length (string-trim-left line)))))
          (setq indent (if indent (min indent n) n)))))
    (if (and indent (> indent 0))
        (mapconcat (lambda (line)
                     (if (>= (length line) indent) (substring line indent) line))
                   lines "\n")
      text)))

(defun rich-text/to-clipboard (text)
  "Convert TEXT from the buffer's markup to rich text on the clipboard.
The source format follows the major mode via `rich-text/pandoc-sources';
signals a `user-error' in modes without an entry.  Conversion runs through
the `rich-text/pandoc-lua' writer, which prepares the HTML for pasting into
Asana."
  (let ((source (or (cdr (seq-find (lambda (cell) (derived-mode-p (car cell)))
                                   rich-text/pandoc-sources))
                    (user-error "rich-text: no rich-text exporter for %s"
                                major-mode))))
    (unless (executable-find "pandoc")
      (user-error "rich-text: pandoc is required to copy rich text"))
    (unless (file-readable-p rich-text/pandoc-lua)
      (user-error "rich-text: %s is missing" rich-text/pandoc-lua))
    (with-temp-buffer
      (insert (if (equal source "org")
                  ;; Keep treating # comment lines as level-2 headings, like
                  ;; the old org-export path did (pandoc drops org comments).
                  (replace-regexp-in-string "^# " "** " text)
                ;; A Markdown region copied from a nested (indented) position
                ;; would otherwise read as an indented code block.
                (rich-text/dedent text)))
      (unless (eq 0 (call-process-region (point-min) (point-max) "pandoc" t t nil
                                         "-f" source
                                         "-t" rich-text/pandoc-lua
                                         "--wrap=none"))
        (user-error "rich-text: pandoc failed to convert the %s" source))
      (rich-text/html-to-clipboard (buffer-string)))))

(defun rich-text/copy-buffer ()
  "Convert the whole buffer to rich text and copy it to the clipboard.
The source format follows the major mode -- Org in `org-mode', Markdown in
`markdown-mode'; other modes signal a `user-error'.  Puts text/html on the
clipboard, so it pastes formatted (links included) into tools like Asana."
  (interactive)
  (rich-text/to-clipboard
   (buffer-substring-no-properties (point-min) (point-max)))
  (message "Copied buffer as rich text."))

(defun rich-text/copy-region (start end)
  "Convert the region to rich text and copy it to the clipboard.
Like `rich-text/copy-buffer', but for the region between START and END;
likewise refuses to run in modes without a rich-text exporter.  A Markdown
region has its common indentation stripped first (see `rich-text/dedent'),
so a selection from a nested position converts at the top level."
  (interactive "r")
  (rich-text/to-clipboard (buffer-substring-no-properties start end))
  (deactivate-mark)
  (message "Copied region as rich text."))

;;; Paste rich text in from the clipboard, converted to the buffer's markup.

(defvar rich-text/pandoc-targets
  '((org-mode      "org" "--wrap=none")
    (markdown-mode "gfm" "--wrap=auto" "--columns=100")
    (rst-mode      "rst" "--wrap=none"))
  "How `rich-text/yank' converts clipboard HTML per major mode:
\(MODE TARGET-FORMAT PANDOC-ARGS...).  Modes are matched with
`derived-mode-p', so derived modes (e.g. `gfm-mode') inherit their parent's
entry; the from-side is always `rich-text/pandoc-lua'.  Markdown is
hard-wrapped at 100 columns, where the source width is also the displayed
width; Org is pasted with one long line per paragraph, because its folded
link URLs make character-based wrapping look ragged -- let the display wrap
it instead (e.g. `visual-line-mode').")

(defun rich-text/yank ()
  "Yank the clipboard's rich text, converted to the current buffer's markup.
When the clipboard holds HTML (e.g. a formatted selection copied from a web
page) convert it with pandoc to the format matching the major mode -- Markdown
in `markdown-mode', Org syntax in `org-mode', reStructuredText in `rst-mode'.
The conversion reads through the `rich-text/pandoc-lua' reader, which repairs
and tidies the HTML (Asana's flat lists included) before pandoc writes the
target markup, wrapped as `rich-text/pandoc-targets' specifies.  With no HTML
on the clipboard, no matching mode, or no usable pandoc setup, fall back to a
plain `yank'.  Any active region is replaced, like a normal yank."
  (interactive)
  (let* ((cell (seq-find (lambda (c) (derived-mode-p (car c)))
                         rich-text/pandoc-targets))
         (html (and cell
                    (executable-find "pandoc")
                    (file-readable-p rich-text/pandoc-lua)
                    (rich-text/clipboard-html))))
    (if (not html)
        (call-interactively #'yank)
      (let* ((exit 1)
             (text (with-temp-buffer
                     (insert html)
                     (setq exit (apply #'call-process-region
                                       (point-min) (point-max) "pandoc" t t nil
                                       "-f" rich-text/pandoc-lua
                                       "-t" (cadr cell)
                                       (cddr cell)))
                     (string-trim (buffer-string)))))
        (if (and (eq exit 0) (not (string-empty-p text)))
            (progn
              (when (use-region-p)
                (delete-region (region-beginning) (region-end)))
              (insert text)
              (message "Pasted rich text as %s." (cadr cell)))
          ;; Pandoc failed or produced nothing -- fall back to plain text.
          (call-interactively #'yank))))))

(provide 'rich-text)
;;; rich-text.el ends here
