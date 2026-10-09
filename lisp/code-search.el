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

(require 'project)
(require 'url-util)

(declare-function embark--mark-target "embark")

(defvar code-search/known-url-builders
  '(code-search/builder-chromium
    code-search/builder-firefox))

(defvar code-search/url-builder nil
  "Function that builds a code-search URL from a context plist.
The plist contains the absolute `:file', project `:root',
`:relative-file', one-based `:line', optional inclusive `:end-line',
and optional `:query'.  For a selection, `:line' is its first line.
Absolute names can be TRAMP names.  The function must return a URL string.")

(put 'code-search/url-builder
     'safe-local-variable
     (lambda (value)
       (and (symbolp value)
            (memq value code-search/known-url-builders))))

(defun code-search--line-range (context)
  "Return CONTEXT's line or inclusive line range as a string."
  (let ((line (plist-get context :line))
        (end-line (plist-get context :end-line)))
    (if (and end-line (> end-line line))
        (format "%d-%d" line end-line)
      (number-to-string line))))

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
       "https://source.chromium.org/chromium/chromium/src/+/main:%s;l=%s"
       (url-hexify-string file (cons ?/ url-unreserved-chars))
       (code-search--line-range context)))))

(defun code-search/builder-firefox (context)
  (let ((file (plist-get context :relative-file))
        (query (plist-get context :query)))
    (if query
        (format "https://searchfox.org/firefox-main/search?q=pathre:%s+%s"
                (url-hexify-string
                 (concat "^" (regexp-quote file) "$"))
                (url-hexify-string query))
      (format "https://searchfox.org/firefox-main/source/%s#%s"
              (url-hexify-string file (cons ?/ url-unreserved-chars))
              (code-search--line-range context)))))

(defun code-search/search (&optional query)
  "Search the current file for QUERY, or visit its location if nil.
Without QUERY, visit the active region's lines or the current line.
The region's end is exclusive.  Interactive calls never prompt for a query."
  (interactive)
  (let* ((file (or (buffer-file-name)
                   (user-error "Code search: Only file buffers supported.")))
         (builder (or code-search/url-builder
                      (user-error
                       "Code search: URL builder not configured for this project")))
         (project (or (project-current nil (file-name-directory file))
                      (user-error
                       "Code search: Project root couldn't be determined.")))
         (root (project-root project))
         (start (and (not query) (use-region-p) (region-beginning)))
         (finish (and start (region-end)))
         (context (list :file file
                        :root root
                        :relative-file (file-relative-name file root)
                        :line (line-number-at-pos start t)
                        :end-line (and finish
                                       (line-number-at-pos
                                        (max start (1- finish)) t))
                        :query query)))
    (unless (functionp builder)
      (user-error "Code search: Invalid URL builder: %S" builder))
    (let ((url (funcall builder context)))
      (unless (stringp url)
        (user-error "Code search: URL builder returned no URL"))
      (browse-url url))))

(defun code-search/embark-search (&rest args)
  "Search or visit Embark's target described by ARGS.
As an around-action hook, ARGS supplies the target type, text and bounds.
Region, expression and defun targets are temporarily selected before
Embark calls this command interactively with no arguments to visit them.
Other targets supply the search query."
  (interactive)
  (if (memq (plist-get args :type) '(region expression defun))
      (let ((transient-mark-mode t))
        (apply #'embark--mark-target args))
    (code-search/search (plist-get args :target))))

(provide 'code-search)
;;; code-search.el ends here
