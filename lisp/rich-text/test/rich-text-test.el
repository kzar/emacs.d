;;; rich-text-test.el --- Tests for rich-text conversion  -*- lexical-binding: t; -*-

;;; Commentary:
;; Integration tests for the Pandoc reader and writer, using HTML copied from
;; an Asana description.  Clipboard access is stubbed so the tests exercise the
;; real public conversion functions without depending on the desktop session.
;;
;; Run from the repository root:
;;   emacs --batch -Q -L lisp/rich-text \
;;     -l lisp/rich-text/test/rich-text-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'cl-lib)
(require 'ert)

(defconst rich-text-test/module-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

(add-to-list 'load-path rich-text-test/module-directory)
(require 'rich-text)

(defconst rich-text-test/fixture-directory
  (expand-file-name "test/fixtures" rich-text-test/module-directory))

(defun rich-text-test/read-fixture (name)
  "Return the contents of fixture NAME."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name rich-text-test/fixture-directory))
    (buffer-string)))

(defun rich-text-test/count-substring (substring text)
  "Return the number of non-overlapping occurrences of SUBSTRING in TEXT."
  (let ((regexp (regexp-quote substring))
        (start 0)
        (count 0))
    (while (string-match regexp text start)
      (setq start (match-end 0)
            count (1+ count)))
    count))

(defun rich-text-test/yank-html (html mode)
  "Convert HTML into the markup used by MODE."
  (with-temp-buffer
    (setq major-mode mode)
    (cl-letf (((symbol-function #'rich-text/clipboard-html)
               (lambda () html)))
      (rich-text/yank))
    (buffer-string)))

(defun rich-text-test/copy-markup (text mode)
  "Convert TEXT from MODE into clipboard HTML and return it."
  (let (html)
    (with-temp-buffer
      (setq major-mode mode)
      (cl-letf (((symbol-function #'rich-text/html-to-clipboard)
                 (lambda (value) (setq html value))))
        (rich-text/to-clipboard text)))
    html))

(defun rich-text-test/pandoc-json (text source)
  "Return Pandoc's JSON AST for TEXT parsed as SOURCE."
  (with-temp-buffer
    (insert text)
    (unless (eq 0 (call-process-region
                   (point-min) (point-max) "pandoc" t t nil
                   "-f" source "-t" "json"))
      (ert-fail (format "Pandoc could not parse %s" source)))
    (buffer-string)))

(defun rich-text-test/check-round-trip (fixture mode source)
  "Check that FIXTURE survives MODE/SOURCE to HTML and back."
  (let* ((expected (string-trim-right
                    (rich-text-test/read-fixture fixture)))
         (html (rich-text-test/copy-markup expected mode))
         (actual (rich-text-test/yank-html html mode)))
    ;; The writer should mark the fragment as editor-native and retain Asana
    ;; object metadata, while ordinary links remain ordinary links.  Of the
    ;; two task links, only the custom-text link has data-title; the canonical
    ;; task-name link must remain dynamic.
    (should (string-match-p "data-pm-slice=\"0 0 \\[\\]\"" html))
    (should (= 2 (rich-text-test/count-substring
                  "data-asana-object=\"1\"" html)))
    (should (string-match-p "data-object-id=\"3333333333333333\"" html))
    (should (= 1 (rich-text-test/count-substring "data-title=" html)))
    (should (string-match-p
             "data-title=\"link to an Asana task\"" html))
    (should-not (string-match-p "__rich_text_asana_dynamic" html))
    (should (string-match-p
             "data-asana-image-asset-id=\"4444444444444444\"" html))
    (should (string-match-p
             "<a href=\"https://example.com\">standard link</a>" html))
    ;; Asana only restores highlighting from its <mark> form.  A generic
    ;; Pandoc <span class="mark"> is semantically similar but is ignored by
    ;; Asana's paste parser.
    (should
     (string-match-p
      (regexp-quote
       (concat "<mark data-highlight-color=\"yellow\""
               " class=\"ProsemirrorEditor-highlight"
               " ProsemirrorEditor-highlight--yellow\""
               " style=\"background-color:"
               " var(--color-richtext-highlight-background, #feedd9);\">"
               "highlighted</mark>"))
      html))
    (should-not
     (string-match-p "<span[^>]*highlight[^>]*>highlighted</span>" html))
    ;; Compare parsed documents instead of serialized markup: Pandoc may choose
    ;; a different but equivalent list marker or table alignment on each pass.
    (should (equal (rich-text-test/pandoc-json expected source)
                   (rich-text-test/pandoc-json actual source)))))

(ert-deftest rich-text-test/asana-html-to-org ()
  (skip-unless (executable-find "pandoc"))
  (should
   (equal
    (rich-text-test/yank-html
     (rich-text-test/read-fixture "asana.html") 'org-mode)
    (string-trim-right (rich-text-test/read-fixture "asana.org")))))

(ert-deftest rich-text-test/asana-html-to-markdown ()
  (skip-unless (executable-find "pandoc"))
  (should
   (equal
    (rich-text-test/yank-html
     (rich-text-test/read-fixture "asana.html") 'markdown-mode)
    (string-trim-right (rich-text-test/read-fixture "asana.md")))))

(ert-deftest rich-text-test/unmatched-mode-defaults-to-markdown ()
  (skip-unless (executable-find "pandoc"))
  (should
   (equal
    (rich-text-test/yank-html
     (rich-text-test/read-fixture "asana.html") 'fundamental-mode)
    (string-trim-right (rich-text-test/read-fixture "asana.md")))))

(ert-deftest rich-text-test/org-round-trip ()
  (skip-unless (executable-find "pandoc"))
  (rich-text-test/check-round-trip "asana.org" 'org-mode "org"))

(ert-deftest rich-text-test/markdown-round-trip ()
  (skip-unless (executable-find "pandoc"))
  (rich-text-test/check-round-trip "asana.md" 'markdown-mode "gfm"))

(provide 'rich-text-test)
;;; rich-text-test.el ends here
