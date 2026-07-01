;;; rich-text.el --- Rich-text conversion to/from the clipboard  -*- lexical-binding: t; -*-

;;; Commentary:
;; Two complementary commands, both leaning on pandoc/pboard tools:
;;   `rich-text/copy-buffer' converts the whole Org or Markdown buffer OUT to
;;   the clipboard as text/html (e.g. to paste into Asana), and `rich-text/yank'
;;   pastes rich text IN from the clipboard, converted to the current buffer's
;;   markup (Markdown in `markdown-mode', Org in `org-mode', etc.).
;;
;; Neither is bound to a key -- invoke with M-x.

;;; Code:

;;; Copy Org/Markdown out to the clipboard as rich text.

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

(defun rich-text/org-to-html (text)
  "Convert the Org TEXT to an HTML fragment for the clipboard."
  (let ((html (org-export-string-as
               (replace-regexp-in-string "^# " "** " text) ; treat # lines as level-2 headings
               'html t '(:with-toc nil))))
    ;; Drop the section numbers Org prepends to headings.
    (replace-regexp-in-string
     "<span class=\"section-number-[0-9]+\">[0-9.]+</span>" "" html)))

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

(defun rich-text/markdown-to-html (text)
  "Convert Markdown (GFM) TEXT to an HTML fragment via pandoc."
  (unless (executable-find "pandoc")
    (user-error "rich-text/copy-buffer: pandoc is required to copy Markdown as rich text"))
  (with-temp-buffer
    (insert (rich-text/dedent text))
    (unless (eq 0 (call-process-region (point-min) (point-max) "pandoc" t t nil
                                       "-f" "gfm" "-t" "html" "--wrap=none"))
      (user-error "rich-text/copy-buffer: pandoc failed to convert the Markdown"))
    (buffer-string)))

(defun rich-text/copy-buffer ()
  "Convert the whole buffer to rich text and copy it to the clipboard.
The source format follows the major mode -- Org in `org-mode', Markdown in
`markdown-mode'.  Puts text/html on the clipboard, so it pastes formatted into
tools like Asana."
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (html (cond
                ((derived-mode-p 'org-mode) (rich-text/org-to-html text))
                ((derived-mode-p 'markdown-mode) (rich-text/markdown-to-html text))
                (t (user-error "rich-text/copy-buffer: no rich-text exporter for %s"
                               major-mode)))))
    (rich-text/html-to-clipboard html)
    (message "Copied buffer as rich text.")))

;;; Paste rich text in from the clipboard, converted to the buffer's markup.

(defvar rich-text/pandoc-args
  '((org-mode      "-f" "html-native_spans-native_divs-auto_identifiers" "-t" "org")
    (markdown-mode "-f" "html-native_spans-native_divs"                  "-t" "gfm")
    (rst-mode      "-f" "html-native_spans-native_divs"                  "-t" "rst"))
  "Pandoc arguments for converting clipboard HTML in each major mode.
Modes are matched with `derived-mode-p', so derived modes (e.g. `gfm-mode')
inherit their parent's entry.  Line wrapping (`--wrap=auto' at
`rich-text/wrap-column' columns) is always added.

Disabling `native_spans'/`native_divs' drops the styling <span>/<div> wrappers
web apps litter their markup with (e.g. Asana's data-* mention spans), keeping
only the content.  For Org we also drop pandoc's auto-generated heading IDs,
which otherwise appear as :PROPERTIES: drawers.")

(defvar rich-text/wrap-column 100
  "Column at which `rich-text/yank' soft-wraps pasted paragraphs.
Wrapping only breaks at spaces and never changes the rendered output: tables,
code, and unbreakable tokens (e.g. a long URL) still spill past this width.")

(defvar rich-text/asciify t
  "When non-nil, `rich-text/yank' replaces smart typography (curly quotes,
em/en dashes, ellipses, non-breaking spaces) with plain ASCII equivalents.")

(defvar rich-text/ascii-replacements
  '(("[“”„‟]" . "\"")   ; curly / low double quotes
    ("[‘’‚‛]" . "'")     ; curly single quotes / apostrophes
    ("—"                     . "---")   ; em dash
    ("–"                     . "--")    ; en dash
    ("…"                     . "...")   ; ellipsis
    (" "                     . " "))    ; non-breaking space -> space
  "Alist of (REGEXP . REPLACEMENT) applied when `rich-text/asciify' is set.")

(defun rich-text/asciify-typography (text)
  "Return TEXT with smart typography flattened to ASCII.
Uses `rich-text/ascii-replacements'."
  (dolist (pair rich-text/ascii-replacements text)
    (setq text (replace-regexp-in-string (car pair) (cdr pair) text t t))))

(defun rich-text/flatten-cell (cell)
  "Return the inner HTML of a table CELL flattened to a single inline line.
Empty paragraphs are dropped and paragraph breaks become spaces, so the cell
holds no block content -- a requirement for Markdown/Org tables, which pandoc
otherwise emits as raw HTML."
  (setq cell (replace-regexp-in-string "<p[^>]*>[ \t\r\n]*</p>" "" cell))
  (setq cell (replace-regexp-in-string "</p>[ \t\r\n]*<p[^>]*>" " " cell))
  (setq cell (replace-regexp-in-string "</?p[^>]*>" "" cell))
  (string-trim cell))

(defun rich-text/rebuild-asana-list (inner)
  "Rebuild Asana's flat list markup INNER into nested <ul>/<ol> HTML.
INNER is the content between an <ol>/<ul> tag and its close.  Asana (and other
Prosemirror editors) emit every list item as a sibling <li> whose nesting depth
and kind live in `data-list-indent'/`data-list-type' attributes rather than in
real <ul>/<ol> nesting -- which pandoc ignores, so every item collapses to one
level.  Reconstruct proper nesting from those attributes.

Return nil -- leaving the list untouched -- unless the items carry
`data-list-indent' (i.e. it is not one of these flat lists)."
  (let ((items '()) (pos 0) (ok t))
    ;; Collect each flat <li> as (INDENT TAG BODY).  These <li> never contain
    ;; other <li>, so a non-greedy match splits them reliably.
    (while (and ok (string-match "<li\\b\\([^>]*\\)>\\(\\(?:.\\|\n\\)*?\\)</li>" inner pos))
      (let ((attrs (match-string 1 inner))
            (body  (match-string 2 inner)))
        (setq pos (match-end 0))
        (if (string-match "data-list-indent=\"\\([0-9]+\\)\"" attrs)
            (let ((indent (string-to-number (match-string 1 attrs)))
                  (type   (if (string-match "data-list-type=\"\\([^\"]*\\)\"" attrs)
                              (match-string 1 attrs) "")))
              (push (list indent (if (string= type "bulleted") "ul" "ol") body) items))
          (setq ok nil))))
    (when (and ok items)
      (setq items (nreverse items))
      ;; Walk the items, opening/closing nested lists as the indent changes.
      ;; STACK holds the open list tags, innermost first; its length is the depth.
      (let ((out "") (stack '()))
        (dolist (it items)
          (let ((indent (nth 0 it)) (tag (nth 1 it)) (body (nth 2 it)))
            (if (> indent (length stack))
                ;; Descend: open new lists inside the still-open parent <li>.
                (while (< (length stack) indent)
                  (setq out (concat out "<" tag ">"))
                  (push tag stack))
              ;; Close the previous sibling, then ascend to this item's depth.
              (setq out (concat out "</li>"))
              (while (> (length stack) indent)
                (setq out (concat out "</" (pop stack) "></li>")))
              ;; Same depth but a different list kind: switch lists.
              (when (and stack (not (string= (car stack) tag)))
                (setq out (concat out "</" (pop stack) "><" tag ">"))
                (push tag stack)))
            (setq out (concat out "<li>" body))))
        ;; Close the final item and every list still open.
        (setq out (concat out "</li>"))
        (while stack
          (setq out (concat out "</" (pop stack) ">"))
          (when stack (setq out (concat out "</li>"))))
        out))))

(defvar rich-text/asana-html-regexp
  "ProsemirrorEditor\\|data-asana-object"
  "Regexp identifying clipboard HTML as coming from Asana's editor.
`rich-text/yank' applies its Asana-specific clean-ups only when the pasted HTML
matches this (see `rich-text/tidy-asana-html'); any other rich text is handed
straight to pandoc untouched.")

(defun rich-text/asana-html-p ()
  "Return non-nil if the current buffer holds Asana rich-text HTML.
Tested with `rich-text/asana-html-regexp'."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward rich-text/asana-html-regexp nil t)))

(defun rich-text/tidy-html ()
  "Tidy the HTML in the current buffer so pandoc converts it to clean markup.
These clean-ups help rich text from any source, not just Asana: drop <br> line
breaks and flatten block content inside table cells (so tables become
Markdown/Org tables rather than raw HTML), strip presentational
class/style/data-* attributes (so e.g. a class-carrying link becomes a Markdown
link instead of leaking as raw HTML), and unwrap the <p> tags editors wrap list
items in (so lists stay tight, not loose).  Asana's own quirks are handled
separately by `rich-text/tidy-asana-html'."
  ;; A line break anywhere in a cell disqualifies the table from being a
  ;; pipe/Org table, so drop <br> (with any attributes) everywhere.
  (goto-char (point-min))
  (while (re-search-forward "<br[^>]*>" nil t)
    (replace-match " " t t))
  ;; Strip presentational attributes (class/style/data-*).  Pandoc preserves an
  ;; unrepresentable attribute by falling back to raw HTML -- e.g. an
  ;; <a class="..." href="..."> is emitted as a literal tag instead of a
  ;; Markdown link.
  (dolist (re '(" class=\"[^\"]*\""
                " style=\"[^\"]*\""
                " data-[a-z0-9-]+=\"[^\"]*\""))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (replace-match "" t t)))
  ;; Editors wrap each list item's text in <p> (<li><p>...</p></li>), which makes
  ;; pandoc emit a "loose" list -- a blank line between every item.  Strip the <p>
  ;; wrappers that sit against list structure so items hold inline content and the
  ;; list stays tight.  Only <p> tags adjacent to <li>/<ul>/<ol> are touched, so
  ;; top-level paragraphs and genuinely multi-paragraph items are left alone.
  (dolist (rule '(("\\(<li\\b[^>]*>\\)[ \t\r\n]*<p\\b[^>]*>" . "\\1")
                  ("</p>[ \t\r\n]*</li>"                     . "</li>")
                  ("</p>[ \t\r\n]*\\(<[ou]l\\b\\)"           . "\\1")
                  ("\\(</[ou]l>\\)[ \t\r\n]*<p\\b[^>]*>"     . "\\1")))
    (goto-char (point-min))
    (while (re-search-forward (car rule) nil t)
      (replace-match (cdr rule) t)))
  ;; Flatten each <td>/<th> cell's contents to inline text.
  (goto-char (point-min))
  (while (re-search-forward
          "\\(<t\\([dh]\\)\\b[^>]*>\\)\\(\\(?:.\\|\n\\)*?\\)\\(</t\\2>\\)" nil t)
    (replace-match
     (concat (match-string 1) (rich-text/flatten-cell (match-string 3))
             (match-string 4))
     t t)))

(defun rich-text/tidy-asana-html ()
  "Reconstruct Asana's flat lists in the current buffer into real nested lists.
Asana (Prosemirror) emits list items as a flat run of <li> siblings whose
nesting lives only in data-list-indent/data-list-type attributes -- which pandoc
ignores, so every item collapses to one level.  Rebuild proper nested <ul>/<ol>
from those attributes.

This is Asana-specific; guard the call with `rich-text/asana-html-p'.  Run it
BEFORE `rich-text/tidy-html', which strips the data-* attributes it relies on.
Lists without data-list-indent are left untouched."
  (when (save-excursion (goto-char (point-min))
                        (search-forward "data-list-indent" nil t))
    (goto-char (point-min))
    (while (re-search-forward
            "<\\(ol\\|ul\\)\\b[^>]*>\\(\\(?:.\\|\n\\)*?\\)</\\1>" nil t)
      (let* ((inner (match-string 2))
             (rebuilt (save-match-data (rich-text/rebuild-asana-list inner))))
        (when rebuilt (replace-match rebuilt t t))))))

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

(defun rich-text/yank ()
  "Yank the clipboard's rich text, converted to the current buffer's markup.
When the clipboard holds HTML (e.g. a formatted selection copied from a web
page) convert it with pandoc to the format matching the major mode -- Markdown
in `markdown-mode', Org syntax in `org-mode', reStructuredText in `rst-mode'.
With no HTML on the clipboard, no matching mode, or no pandoc, fall back to a
plain `yank'.  Any active region is replaced, like a normal yank."
  (interactive)
  (let* ((cell (seq-find (lambda (c) (derived-mode-p (car c)))
                         rich-text/pandoc-args))
         (html (and cell (executable-find "pandoc") (rich-text/clipboard-html))))
    (if (not html)
        (call-interactively #'yank)
      (let* ((exit 1)
             (text (with-temp-buffer
                     (insert html)
                     ;; Asana-specific fix-ups first (they need the data-* attrs
                     ;; the general tidy strips), then the general clean-ups.
                     (when (rich-text/asana-html-p)
                       (rich-text/tidy-asana-html))
                     (rich-text/tidy-html)
                     (setq exit (apply #'call-process-region
                                       (point-min) (point-max) "pandoc"
                                       t t nil "--wrap=auto"
                                       (format "--columns=%d" rich-text/wrap-column)
                                       (cdr cell)))
                     (string-trim (buffer-string)))))
        (when rich-text/asciify
          (setq text (rich-text/asciify-typography text)))
        (if (and (eq exit 0) (not (string-empty-p text)))
            (progn
              (when (use-region-p)
                (delete-region (region-beginning) (region-end)))
              (insert text)
              (message "Pasted rich text as %s."
                       (or (cadr (member "-t" (cdr cell))) "text")))
          ;; Pandoc failed or produced nothing -- fall back to plain text.
          (call-interactively #'yank))))))

(provide 'rich-text)
;;; rich-text.el ends here
