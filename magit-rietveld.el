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

;;; Code:

;; FIXME - Mark incompatible with magit-gerrit

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

; Create the code review
; https://codereview.adblockplus.org/upload - form-data
; repo_guid, subject, description, base_hashes, data (filename=data.diff)

; base = current repository name / url

(defun get-first-root-hash ()
 (with-temp-buffer
   (magit-git-insert "rev-list" "--parents" "HEAD")
   (and (search-backward-regexp "\\W\\([[:alnum:]]+\\)" nil t)
        (match-string 1))))

(setq diff "\
Index: include.preload.js
diff --git a/include.preload.js b/include.preload.js
index 3953332b0d54f35c8d1a7c2e185ee52dad2bbdfc..219401018781a6a774cbc61f388f738076dde874 100644
--- a/include.preload.js
+++ b/include.preload.js
@@ -300,7 +300,7 @@ function init(document)
     shadow.appendChild(document.createElement(\"shadow\"));
   }
 
-  var addElemHideSelectors = function(selectors)
+  function addElemHideSelectors(selectors)
   {
     if (selectors.length == 0)
       return;
")
(setq base-hashes "11d0f6c0b12c4a09730bf84d36067057:include.preload.js|537b1465d3d2b680981d2da9d9332853:metadata.common")

(let* ((url-request-method "POST")
       (boundary "--M-A-G-I-T--R-I-E-T-V-E-L-D--")
       (url-request-extra-headers
        `(("Content-Type" . ,(concat "multipart/form-data; boundary=" boundary))
          ("Authorization" . ,(concat "OAuth " auth-token))))
       (subject (read-from-minibuffer "Subject: "))
       (fields `(("repo_guid" . ,(get-first-root-hash))
                 ("subject" . ,subject)
                 ("description" . ,subject)
                 ("base_hashes" . ,base-hashes)
                 ("content_upload" . "1")
                 ("base" . "Hello-world")))
       (files `(("data" "data.diff" "text/x-diff" ,diff)))
       (url-request-data (encode-multipart-form-data boundary fields files)))
  (url-retrieve-synchronously (concat magit-rietveld-server "/upload")))

(defun guess-rev ()
  (let ((selected (magit-region-values))
        (current (magit-section-value (magit-current-section)))
        (branch (magit-get-current-branch)))
    (if (eq (magit-section-type (magit-current-section)) 'commit)
        (if (> (length selected) 1)
            (concat (last selected) "~:" (first selected))
          (concat current "~:" current))
      ; FIXME - Check Branch / HEAD is after Master!
      (concat "master:" (or branch "HEAD")))))

(defun generate-diff (rev-start rev-end)
  (with-temp-buffer
    (let ((args '("diff" "--no-color" "--no-ext-diff" "--full-index"
                  "--ignore-submodules" "--src-prefix=a/" "--dst-prefix=b/")))
      (apply 'magit-git-insert (append args '("--no-renames" "--diff-filter=D")
                                       `(,rev-start ,rev-end)))
      (apply 'magit-git-insert (append args '("--diff-filter=AMCRT" "-M50%")
                                       `(,rev-start ,rev-end)))
      (beginning-of-buffer)
      ; TODO - take note of index x..y hashes for later?
      (while (search-forward-regexp "diff --git a/\\(.*\\) b/\\(.*\\)$" nil t)
        (let ((filename (match-string 2)))
          (move-beginning-of-line 1)
          (insert (concat "Index: ") filename "\n")
          (move-end-of-line 2)))
      (buffer-string))))

(generate-diff "HEAD~" "HEAD")

(defun magit-rietveld-submit-review ()
  (interactive)
  (let ((rev (read-from-minibuffer "Revision: " (guess-rev)))
        (subject (read-from-minibuffer "Subject: ")))
    ; FIXME - Don't submit --rev argument if falsey, submit other arguments
    (upload.py "--rev" rev)))

;(message "hello")

; FIXME - Make use of (magit-diff "rev..rev") to display a diff before
; submitting?

; If (eq (magit-section-type (magit-current-section)) 'commit)
;  If (magit-region-values) - Take the first and last as the range
;  Else (magit-section-value (magit-current-section))
; Else "HEAD
(magit-rev-parse (magit-rev-parse "HEAD"))
(magit-region-sections)
; (magit-region-values)

(magit-define-popup magit-rietveld-popup
  "Popup console for magit rietveld commands."
  'magit-rietveld
  :actions '((?R "Submit code review" magit-rietveld-submit-review))
  :options '())

(magit-define-popup-action 'magit-dispatch-popup ?R "Rietveld"
  'magit-rietveld-popup)
