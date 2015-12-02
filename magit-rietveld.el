;; -*- lexical-binding: t -*-
;;; magit-rietveld.el --- Magit plugin for the Rietveld Code Review system

;; Copyright (C) 2015 Dave Barker

;; Author: Dave Barker <kzar@kzar.co.uk>

;; Package-Requires: ((magit "2.1.0") (oauth2 "0.10"))
;; Keywords: git tools vc rietveld upload.py
;; Homepage: https://github.com/kzar/magit-rietveld

;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with it.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; magit-rietveld.el is an interface for interacting with the Rietveld
;; code review system. The code is a elisp port of a small subset of the
;; upload.py script included with Rietveld, which will allow for more
;; convenient magit integration. Parts of this file are also derived from
;; the magit-gerrit code.

;;; Usage:

;; FIXME - write usage instructions!

;;; Code:

(require 'magit)

; FIXME - this should not be the default
(defvar-local magit-rietveld-server "https://codereview.adblockplus.org")

(setq auth-token nil)
(setq redirect-response "\
HTTP/1.1 200 OK\r
\r
<html>
  <head>
    <script>window.onload = function () { window.close(); };</script>
  </head>
  <body>
    <p>Rietveld access token obtained, you can close this page.</p>
  </body>
</html>")

(defun oauth-redirect-filter (connection string)
  (with-temp-buffer
    (insert string)
    (setq auth-token
          (and
           (search-backward-regexp "access_token=\\([^&[:space:]]+\\)" nil t)
           (match-string 1)))
    (process-send-string connection redirect-response)
    (delete-process connection)
    (delete-process "oauth-redirect")))

(defun get-auth-token ()
  (make-network-process :name "oauth-redirect" :server t
                        :service 8001 :family 'ipv4
                        :filter 'oauth-redirect-filter)
  (browse-url (concat magit-rietveld-server "/get-access-token?port=8001"))
  (run-at-time 10 nil (lambda ()
                        (when (process-status "oauth-redirect")
                          (minibuffer-message
                           "Timed out waiting for Rietveld oauth2 access token")
                          (delete-process "oauth-redirect")))))
; FIXME - block waiting for this to finish, return the code or nil on timeout
;       (Perhaps we should prompt for the auth code if the redirect flow fails?)

(get-auth-token)

(defun encode-multipart-form-data (boundary fields files)
  (with-temp-buffer
    (dolist (field fields)
      (insert "--" boundary "\r\n")
      (insert "Content-Disposition: form-data; name=\""
              (car field) "\"" "\r\n" "\r\n")
      (insert (cdr field) "\r\n"))
    (dolist (file files)
      (insert "--" boundary "\r\n")
      (insert "Content-Disposition: form-data; name=\"" (car file) "\"; "
              "filename=\"" (cadr file) "\"\r\n")
      (insert "Content-Type: " (caddr file) "\r\n" "\r\n")
      (insert (cadddr file)))
    (insert "--" boundary "--\r\n\r\n")
    (buffer-string)))

(defun get-first-root-hash ()
 (with-temp-buffer
   (magit-git-insert "rev-list" "--parents" "HEAD")
   (and (search-backward-regexp "\\W\\([[:alnum:]]+\\)" nil t)
        (match-string 1))))

(defun guess-revision-range ()
  (let* ((selected (magit-region-values))
         (branch (magit-get-current-branch))
         (current (magit-current-section)))
    (if (and current (eq (magit-section-type current) 'commit))
        (let ((current-rev (magit-section-value current)))
          (if (> (length selected) 1)
              (concat (car (last selected)) ":" (first selected))
            (concat current-rev "~:" current-rev)))
      (concat "master:" (or branch "HEAD")))))

(defun revision-is-ancestor? (ancestor revision)
  (let ((ancestor (magit-rev-parse ancestor)))
    (with-temp-buffer
      (magit-git-insert "rev-list" "--parents" revision)
      (beginning-of-buffer)
      (and (search-forward-regexp (rx-to-string ancestor) nil t)
           (not (string= ancestor revision))))))

(defun valid-revision-range? (range)
  (let ((revisions (mapcar 'magit-rev-verify (split-string range ":"))))
    (and (= (length revisions) 2) (every 'identity revisions)
         (revision-is-ancestor? (car revisions) (cadr revisions)))))

(defun prompt-for-revision-range ()
  (let ((current (guess-revision-range)))
    (while (not (valid-revision-range?
                 (setq current (read-from-minibuffer "Revision: " current)))))
    current))

(defun prompt-for-issue-number (&rest args)
  (let ((current ""))
    (while (not (string-match (rx bos (+ digit) eos) current))
      (setq current (read-from-minibuffer "Issue number: ")))
    current))

(defun strip-null-hash (hash)
  (let ((null-hash "0000000000000000000000000000000000000000"))
    (and (not (string= hash null-hash)) hash)))

; FIXME - Is there no way to avoid all this global state? :(
(setq filenames ())
(setq statuses (make-hash-table :test 'equal))
(setq hashes (make-hash-table :test 'equal))
(setq renames (make-hash-table :test 'equal))
(setq base-contents (make-hash-table :test 'equal))
(setq new-contents (make-hash-table :test 'equal))
(setq binary-ps (make-hash-table :test 'equal))

(defun reset-diff ()
  (setq filenames ())
  (mapc 'clrhash `(,statuses ,hashes ,renames ,base-contents
                   ,new-contents ,binary-ps)))

(defun is-image? (filename)
  (and (string-match
        (rx "." (or "bmp" "gif" "ief" "jpe" "jpeg" "jpg" "pbm" "pgm" "png" "pnm"
                    "ppm" "ras" "rgb" "tif" "tiff" "xbm" "xpm" "xwd" "jpg" "pct"
                    "pic" "pict")
            eos)
        filename)
       t))

(defun is-binary? (contents)
  (and (search "\0" contents) t))

(defun get-file-content (hash)
  (with-temp-buffer
    (magit-git-insert "git" "show" hash)
    (buffer-string)))

(defun diff (rev-start rev-end)
  (reset-diff)
  (with-temp-buffer
    ; Insert the Git diff
    (let ((args '("diff" "--no-color" "--no-ext-diff" "--full-index"
                  "--ignore-submodules" "--src-prefix=a/" "--dst-prefix=b/")))
      (apply 'magit-git-insert (append args '("--no-renames" "--diff-filter=D")
                                       `(,rev-start ,rev-end)))
      (apply 'magit-git-insert (append args '("--diff-filter=AMCRT" "-M50%")
                                       `(,rev-start ,rev-end))))
    ; Add Index: ... lines
    (beginning-of-buffer)
    (while (search-forward-regexp "^diff --git a/\\(.*\\) b/\\(.*\\)$" nil t)
      (let ((filename-before (match-string 1))
            (filename (match-string 2)))
        (push filename filenames)
        ; Keep track of weather or not this file has been renamed
        (let ((renamed (not (string= filename-before filename))))
          (when renamed
            (puthash filename filename-before renames))
          (move-beginning-of-line 1)
          (insert (concat "Index: ") filename "\n")
          (move-end-of-line 2)
          ; Keep track of before & after hashes + statuses for this file
          (search-forward-regexp "^index \\(\\w+\\)\\.\\.\\(\\w+\\)$")
          (let ((hash-before (match-string 1))
                (hash-after (match-string 2)))
            (puthash filename `(,(strip-null-hash hash-before) .
                                ,(strip-null-hash hash-after))
                     hashes)
            (puthash filename (cond (renamed "A +")
                                    ((not hash-before) "A")
                                    ((not hash-after) "D")
                                    (t "M"))
                   statuses)
            ; Keep track of base contents for the file
            (let* ((base (cond (renamed (get-file-content
                                         (concat "HEAD:" filename)))
                               ((not hash-before) "")
                               (t (get-file-content filename))))
                   (base-binary (or (is-image? filename)
                                    (is-binary? base))))
              (puthash filename base base-contents)
              ; Keep track of new content if required
              (when hash-after
                (let* ((new (get-file-content hash-after))
                       (binary-p (or base-binary (is-binary? new))))
                  (when binary-p
                    (puthash filename t binary-ps)
                    (puthash filename new new-contents)))))))))
    ; Finally return the diff itself!
    (buffer-string)))
; FIXME - Test all this: renames, permission changes, deletions, additions
;                        modifications, binary files, image files, svg files
;                        huge patch sets
; FIXME - Port the logic to split up huge patch sets from upload.py

(defun base-hashes ()
  (let ((hashes ()))
    (dolist (filename filenames)
      (let ((contents (gethash filename base-contents)))
        (when contents
          (push (concat filename ":" (md5 contents)) hashes))))
    (string-join hashes "|")))

(defun parse-args (args)
  (mapcar (lambda (arg)
            (let ((parts (split-string arg "=" nil (rx bos "--"))))
              `(,(car parts) . ,(cadr parts))))
          args))

(defun magit-rietveld-submit-review ()
  (interactive)
  (let* ((args (parse-args (magit-rietveld-arguments)))
         (rev (prompt-for-revision-range))
         (subject (read-from-minibuffer "Subject: "))
         (diff-output (apply 'diff (split-string rev ":")))
         ; Set up the web request
         (url-request-method "POST")
         (boundary "--M-A-G-I-T--R-I-E-T-V-E-L-D--")
         (url-request-extra-headers
          `(("Content-Type" . ,(concat "multipart/form-data; boundary="
                                       boundary))
            ("Authorization" . ,(concat "OAuth " auth-token))))
         (fields `(("repo_guid" . ,(get-first-root-hash))
                   ("subject" . ,subject)
                   ("description" . ,subject)
                   ("base_hashes" . ,(base-hashes))
                   ("content_upload" . "1")
                   ("base" . "FIXME: reponame @ base-rev")))
         (files `(("data" "data.diff" "text/x-diff" ,diff-output))))
    (when-let ((issue (assoc "issue" args)))
      (push issue fields))
    (let ((url-request-data (encode-multipart-form-data boundary fields files)))
      (url-retrieve-synchronously (concat magit-rietveld-server "/upload")))))
; FIXME - Make use of (magit-diff "rev..rev") to display a diff before
;         submitting?

(magit-define-popup magit-rietveld-popup
  "Popup console for magit rietveld commands."
  'magit-rietveld
  :actions '((?r "Submit code review" magit-rietveld-submit-review))
  :options '((?i "Issue number" "--issue=" prompt-for-issue-number)))
; FIXME - Add options for some of upload.py features:
;         specifying issue number, private, ...

(magit-define-popup-action 'magit-dispatch-popup [?\C-c ?r] "Rietveld"
  'magit-rietveld-popup)

(define-key magit-mode-map [?\C-c ?r] 'magit-rietveld-popup)
