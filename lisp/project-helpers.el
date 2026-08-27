;;; project-helpers.el --- Project navigation and settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Workarounds/improvements to better handle large projects like Chromium. Some
;; issues included:
;; - Large codebases are too slow to search with the standard find-file etc.
;;   tools.
;; - Chromium is managed by gclient, and contains several third-party
;;   dependencies as separate nested Git repositories. Those were wrongly
;;   considered as separate projects, so wouldn't show up in file searches, or
;;   share language servers etc.
;; - Both Firefox and Chromium projects include a copy of clangd to use, that is
;;   often newer than the system installed version, failing to use the right one
;;   caused a lot of warnings/noise.

;;; Code:

(require 'project)

(defvar consult-fd-args)
(defvar consult-fd-directory-args)

(declare-function consult-fd "consult" (&optional dir initial))
(declare-function consult-ripgrep "consult" (&optional dir initial))

(defun project-helpers/gclient-checkout-roots (project-root)
  "Return PROJECT-ROOT and its gclient-managed Git checkout roots.
The checkout list comes from `.gclient_entries'.  Entries containing a
colon describe non-Git dependencies and are omitted."
  (let* ((project-root (file-name-as-directory
                        (expand-file-name project-root)))
         (gclient-root (locate-dominating-file project-root
                                               ".gclient_entries")))
    (if (not gclient-root)
        (list project-root)
      (let ((prefix (file-relative-name project-root gclient-root))
            (roots (list project-root)))
        (with-temp-buffer
          (insert-file-contents (expand-file-name ".gclient_entries"
                                                  gclient-root))
          (while (re-search-forward "^[[:space:]]*'\\([^':]+\\)':" nil t)
            (let ((checkout (match-string 1)))
              (when (string-prefix-p prefix checkout)
                (push (file-name-as-directory
                       (expand-file-name checkout gclient-root))
                      roots)))))
        (nreverse roots)))))

(defun project-helpers/search (command)
  "Run Consult search COMMAND across the current project's checkouts."
  (let* ((root (file-name-as-directory
                (expand-file-name (project-root (project-current t)))))
         (default-directory root))
    (funcall command (project-helpers/gclient-checkout-roots root))))

(defun project-helpers/find-file ()
  "Find a file across the current project's checkouts."
  (interactive)
  (require 'consult)
  (project-helpers/search #'consult-fd))

(defun project-helpers/find-regexp ()
  "Search file contents across the current project's checkouts."
  (interactive)
  (require 'consult)
  (project-helpers/search #'consult-ripgrep))

(defun project-helpers/find-dir ()
  "Find a directory across the current project's checkouts."
  (interactive)
  (require 'consult)
  (let ((consult-fd-args consult-fd-directory-args))
    (project-helpers/search #'consult-fd)))

(defvar project-helpers/gclient-root-cache (make-hash-table :test 'equal)
  "Cache mapping Git checkout roots to their gclient solution roots.")

(defun project-helpers/gclient-calculate-root (vc-root)
  "Return the gclient solution root containing VC-ROOT, or nil."
  (when-let* ((gclient-root (locate-dominating-file vc-root ".gclient"))
              (solution-name (car (split-string
                                   (file-relative-name vc-root gclient-root)
                                   "/" t)))
              (source-root (file-name-as-directory
                            (expand-file-name solution-name gclient-root))))
    (and (file-exists-p (expand-file-name ".git" source-root))
         source-root)))

(defun project-helpers/gclient-root (vc-root)
  "Return the gclient solution root containing VC-ROOT, or VC-ROOT."
  (or (gethash vc-root project-helpers/gclient-root-cache)
      (puthash vc-root
               (or (project-helpers/gclient-calculate-root vc-root)
                   vc-root)
               project-helpers/gclient-root-cache)))

(defun project-helpers/gclient-project (directory)
  "Return a gclient project containing DIRECTORY, or nil."
  (when-let* ((vc-project (project-try-vc directory))
              (vc-root (and (eq (nth 1 vc-project) 'Git)
                            (nth 2 vc-project)))
              (source-root (project-helpers/gclient-root vc-root)))
    (unless (equal source-root vc-root)
      (list 'vc 'Git source-root))))

(defvar project-helpers/root-dir-locals--resolving nil
  "Non-nil while resolving project-root directory-local variables.")

(defun project-helpers/root-dir-locals ()
  "Return the directory-local variables governing the project root.
This is intended for `hack-dir-local-get-variables-functions', so the
settings that apply at the project root supplement the nearest nested
`.dir-locals.el'.  Emacs gives nearer settings precedence when both
define the same variable."
  (unless project-helpers/root-dir-locals--resolving
    (let ((project-helpers/root-dir-locals--resolving t))
      (when-let* ((project (project-current))
                  (root (file-name-as-directory
                         (expand-file-name (project-root project))))
                  (entry (dir-locals-find-file root))
                  (locals-root
                   (file-name-as-directory
                    (expand-file-name
                     (if (stringp entry) entry (car entry)))))
                  (class (if (stringp entry)
                             (dir-locals-read-from-dir locals-root)
                           (nth 1 entry))))
        (cons locals-root
              (dir-locals-collect-variables
               (dir-locals-get-class-variables class)
               locals-root nil))))))

(defvar-local clangd-executable nil
  "Project-provided path to the clangd executable.")

(defun project-helpers/clangd-path ()
  "Return the project clangd path or fall back to the installed version."
  (if-let* ((path
             (and (stringp clangd-executable)
                  (file-truename
                   (concat (file-remote-p default-directory)
                           clangd-executable))))
            ((file-executable-p path)))
      (file-local-name path)
    "clangd"))

(defun project-helpers/copy-file-path ()
  "Copy the current file's path relative to its project root."
  (interactive)
  (let* ((file (or buffer-file-name
                   (user-error "Current buffer is not visiting a file.")))
         (project (let ((non-essential nil))
                    (project--find-in-directory
                     (file-name-directory file)))))
    (unless project
      (user-error "Current file is not in a project."))
    (let ((path (file-relative-name file (project-root project))))
      (kill-new path)
      (message "Copied path: %s" path))))

(add-hook 'project-find-functions #'project-helpers/gclient-project -10)
(add-hook 'hack-dir-local-get-variables-functions
          #'project-helpers/root-dir-locals t)

(keymap-set project-prefix-map "f" #'project-helpers/find-file)
(keymap-set project-prefix-map "g" #'project-helpers/find-regexp)
(keymap-set project-prefix-map "d" #'project-helpers/find-dir)
(keymap-unset project-prefix-map "s")
(keymap-unset project-prefix-map "e")

(when-let ((entry (assq 'project-find-file project-switch-commands)))
  (setcar entry #'project-helpers/find-file))
(when-let ((entry (assq 'project-find-regexp project-switch-commands)))
  (setcar entry #'project-helpers/find-regexp))
(when-let ((entry (assq 'project-find-dir project-switch-commands)))
  (setcar entry #'project-helpers/find-dir))
(dolist (command '(project-shell project-eshell))
  (setq project-switch-commands
        (assq-delete-all command project-switch-commands)))

(provide 'project-helpers)
;;; project-helpers.el ends here
