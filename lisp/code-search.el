;;; -*- lexical-binding: t; -*-

;; Search the current file in a project's code search website. Configured for
;; projects with a .dir-locals.el, for example:
;;
;;   ;; Chromium
;;   ((nil . ((code-search/url-builder .
;;             code-search/builder-chromium))))
;;
;;   ;; Firefox
;;   ((nil . ((code-search/url-builder .
;;             code-search/builder-firefox))))

(require 'thingatpt)
(require 'project)
(require 'url-util)

(defvar code-search/known-url-builders
  '(code-search/builder-chromium
    code-search/builder-firefox))

(defvar code-search/url-builder nil
  "Function that builds a code-search URL from a context plist.
The plist contains the absolute `:file', project `:root',
`:relative-file', one-based `:line', and optional `:query'.  Absolute
names can be TRAMP names.  The function must return a URL string.")

(put 'code-search/url-builder
     'safe-local-variable
     (lambda (value)
       (and (symbolp value)
            (memq value code-search/known-url-builders))))

(defun code-search/builder-chromium (context)
  (let ((file (plist-get context :relative-file))
        (query (plist-get context :query)))
    (when (string-match "\\`out/[^/]+/gen/" file)
      (setq file (replace-match "out/linux-Debug/gen/" t t file)))
    (if query
        (format
         "https://source.chromium.org/search?q=file:%s+%s&ss=chromium/chromium/src"
         (url-hexify-string (concat "^" (regexp-quote file) "$"))
         (url-hexify-string query))
      (format
       "https://source.chromium.org/chromium/chromium/src/+/main:%s;l=%d"
       (url-hexify-string file (cons ?/ url-unreserved-chars))
       (plist-get context :line)))))

(defun code-search/builder-firefox (context)
  (let ((file (plist-get context :relative-file))
        (query (plist-get context :query)))
    (if query
        (format "https://searchfox.org/mozilla-central/search?q=pathre:%s+%s"
                (url-hexify-string
                 (concat "^" (regexp-quote file) "$"))
                (url-hexify-string query))
      (format "https://searchfox.org/mozilla-central/source/%s#%d"
              (url-hexify-string file (cons ?/ url-unreserved-chars))
              (plist-get context :line)))))

(defun code-search--search (query)
  "Search the current file for QUERY, or visit its current line if nil."
  (let* ((file (or (buffer-file-name)
                   (user-error "Code search: Only file buffers supported.")))
         (builder (or code-search/url-builder
                      (user-error
                       "Code search: URL builder not configured for this project")))
         (project (or (project-current nil (file-name-directory file))
                      (user-error
                       "Code search: Project root couldn't be determined.")))
         (root (project-root project))
         (context (list :file file
                        :root root
                        :relative-file (file-relative-name file root)
                        :line (line-number-at-pos nil t)
                        :query query)))
    (unless (functionp builder)
      (user-error "Code search: Invalid URL builder: %S" builder))
    (let ((url (funcall builder context)))
      (unless (stringp url)
        (user-error "Code search: URL builder returned no URL"))
      (browse-url url))))

(defun code-search/search ()
  "Search for the region or symbol, or visit the current file and line."
  (interactive)
  (code-search--search
   (if (use-region-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun code-search/embark-search (query)
  "Search the current file for Embark's QUERY target."
  (interactive "sSearch query: ")
  (code-search--search query))

(provide 'code-search)
;;; code-search.el ends here
