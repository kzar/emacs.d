;;; -*- lexical-binding: t; -*-

;; Use a custom file, but make it optional and keep it out of the repo.
;; Note: Needs to happen before packages are loaded.
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file t)

;; Setup packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package)
(setq use-package-always-ensure t)

;; Load local libraries.
(eval-and-compile
  (dolist (dir '("lisp" "lisp/rich-text" "lisp/chromium"))
    (add-to-list 'load-path (expand-file-name dir user-emacs-directory))))
(require 'my-helpers)
(require 'code-search)
(use-package rich-text
  :ensure nil
  :commands (rich-text/copy-buffer rich-text/copy-region rich-text/yank))

;; Set my username etc.
(setq user-full-name "Dave Vandyke"
      user-mail-address "kzar@kzar.co.uk")

;; Editor basics.
(setq inhibit-splash-screen t
      initial-scratch-message ";; Hello Dave\n")

(prefer-coding-system 'utf-8)

(dolist (dir '("/opt/homebrew/bin"
               "/opt/homebrew/opt/llvm/bin"
               "/usr/local/bin"
               "/usr/local/opt/llvm/bin"
               "~/.cargo/bin"
               "~/Davebox/work/personal/chromium/depot_tools"))
  (let ((dir (expand-file-name dir)))
    (when (file-directory-p dir)
      (add-to-list 'exec-path dir)
      (setenv "PATH" (concat dir path-separator (getenv "PATH"))))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(column-number-mode 1)
(pixel-scroll-precision-mode 1)
(undelete-frame-mode 1)
(which-key-mode 1)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(global-so-long-mode 1)

(editorconfig-mode 1)
(winner-mode 1)
(repeat-mode 1)
(kill-ring-deindent-mode 1)

;; Highlight matching parens green.
(show-paren-mode 1)
(setq show-paren-delay 0
      show-paren-context-when-offscreen 'overlay)
(set-face-background 'show-paren-match "#99FF00")

;; Set default code indent to 2 spaces.
(setq-default tab-width 2
              indent-tabs-mode nil)

;; No lock files (they confuse tools that watch for file changes).
(setq create-lockfiles nil)

;; Tool bar with icons + text labels.
(setq tool-bar-style 'both)

;; Make highlighted text yankable (primary selection).
(setq select-enable-primary t)

;; Short y/n answers, mouse context menus, isearch match counter.
(setq use-short-answers t
      isearch-lazy-count t)
(context-menu-mode 1)

;; Persist history and cursor positions across sessions.
(savehist-mode 1)
(save-place-mode 1)

;; Completion UI
(use-package vertico
  :init (vertico-mode)
  :custom (vertico-cycle t)
  :config
  (keymap-set vertico-map "C-c g" (kbd "C-. g")))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Search and navigation.
(use-package consult
  :commands (consult-completion-in-region consult-fd consult-ripgrep)
  :defines consult-source-buffer
  :bind (("C-x b" . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g f" . consult-flymake)
         ("M-s l" . consult-line)
         ("M-s r" . consult-ripgrep))
  :custom
  (consult-buffer-sources '(consult-source-buffer))
  :init
  (setopt completion-in-region-function #'consult-completion-in-region
          xref-search-program 'ripgrep
          xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :config
  (setf (plist-get consult-source-buffer :name) nil))

;; Actions for completion candidates and things at point.
(use-package embark
  :bind (("C-." . embark-act))
  :config
  (keymap-set embark-identifier-map "C" #'code-search/embark-search)
  (keymap-set embark-region-map "C" #'code-search/embark-search)
  (keymap-set embark-expression-map "C" #'code-search/embark-search)
  (keymap-set
   embark-file-map "g"
   (lambda (path)
     "Open Magit for PATH or the directory containing it."
     (interactive "fPath: ")
     (let ((path (expand-file-name path)))
       (magit-status-setup-buffer
        (if (file-directory-p path)
            path
          (file-name-directory path)))))))

(use-package embark-consult
  :after (embark consult))

;; In-buffer completion UI.
(use-package corfu
  :defer t
  :hook
  (eglot-managed-mode .
   (lambda ()
     (if (eglot-managed-p)
         (progn
           (setq-local corfu-auto t)
           (corfu-mode 1))
       (corfu-mode -1)
       (kill-local-variable 'corfu-auto))))
  :custom
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.5 . 0.5))
  :config
  (corfu-popupinfo-mode 1))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file t))

;; Projects

;; Faster searching within projects using fd/ripgrep.
(defvar consult-fd-args)
(defun kzar/project-find-file ()
  (interactive)
  (consult-fd (project-root (project-current t))))

(defun kzar/project-find-regexp ()
  (interactive)
  (consult-ripgrep (project-root (project-current t))))

(defun kzar/project-find-dir ()
  (interactive)
  (let ((consult-fd-args
         '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
           "--full-path --color=never --type=d")))
    (consult-fd (project-root (project-current t)))))

(with-eval-after-load 'project
  (keymap-set project-prefix-map "f" #'kzar/project-find-file)
  (keymap-set project-prefix-map "g" #'kzar/project-find-regexp)
  (keymap-set project-prefix-map "d" #'kzar/project-find-dir)
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (when-let ((e (assq 'project-find-file   project-switch-commands)))
    (setcar e #'kzar/project-find-file))
  (when-let ((e (assq 'project-find-regexp project-switch-commands)))
    (setcar e #'kzar/project-find-regexp))
  (when-let ((e (assq 'project-find-dir    project-switch-commands)))
    (setcar e #'kzar/project-find-dir))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

;; Ensure gclient managed projects (e.g. Chromium) are treated as one project.
(defvar kzar/gclient-root-cache (make-hash-table :test 'equal))

(defun kzar/gclient-calculate-root (vc-root)
  (when-let* ((gclient-root (locate-dominating-file vc-root ".gclient"))
              (solution-name (car (split-string
                                   (file-relative-name vc-root gclient-root)
                                   "/" t)))
              (source-root (file-name-as-directory
                            (expand-file-name solution-name gclient-root))))
    (and (file-exists-p (expand-file-name ".git" source-root))
         source-root)))

(defun kzar/gclient-root (vc-root)
  (or (gethash vc-root kzar/gclient-root-cache)
      (puthash vc-root
               (or (and (not (file-remote-p vc-root))
                        (kzar/gclient-calculate-root vc-root))
                   vc-root)
               kzar/gclient-root-cache)))

(defun kzar/gclient-project (dir)
  (when-let* ((vc-project (project-try-vc dir))
              (vc-root (and (eq (nth 1 vc-project) 'Git) (nth 2 vc-project)))
              (source-root (kzar/gclient-root vc-root)))
    (unless (equal source-root vc-root) (list 'vc 'Git source-root))))

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'kzar/gclient-project -10))

;; Resolve symlinks when opening files, to ensure they are grouped by project
;; consistently.
(setq find-file-visit-truename t)

;; Zap up to (not including) a char.
(keymap-global-set "M-z" #'zap-up-to-char)

;; Highlight tabs and trailing whitespace red.
(setq whitespace-style '(face trailing tabs space-mark)
      whitespace-global-modes '(not rcirc-mode magit-mode))
(global-whitespace-mode)
(set-face-attribute 'whitespace-tab nil :background "red" :foreground "white")
;; Also highlight non-ASCII characters.
(setq whitespace-tab-regexp "\\([\t[:nonascii:]]\\)")
;; Display zero-width Unicode characters as standard spaces so we don't miss them.
(setq whitespace-display-mappings '((space-mark ?\x200B [? ])
                                    (space-mark ?\x200C [? ])
                                    (space-mark ?\x200D [? ])
                                    (space-mark ?\xFEFF [? ])))

;; Display a long-line indicator for code.
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; Ensure it applies to code files, even if there's no applicable major mode.
(dolist (pattern '("\\.webidl\\'" "/OWNERS\\'"))
  (add-to-list 'auto-mode-alist (cons pattern #'prog-mode)))

;; Spell-checking
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-c C-M-i" . jinx-correct))
  :custom (jinx-languages "en_GB")
  :config
  (set-face-attribute 'jinx-misspelled nil
                      :underline '(:style wave :color "Red1")))

;; Fonts
(defvar kzar/gui-frame-setup-done nil
  "Non-nil once `kzar/gui-frame-hook' has run in a graphical frame.")

(defun kzar/setup-gui-frame (&optional frame)
  "Run `kzar/gui-frame-hook' once, in the first graphical FRAME."
  (when (and (not kzar/gui-frame-setup-done)
             (display-graphic-p frame))
    (setq kzar/gui-frame-setup-done t)
    (with-selected-frame (or frame (selected-frame))
      (run-hooks 'kzar/gui-frame-hook))))

(add-hook 'after-make-frame-functions #'kzar/setup-gui-frame) ; emacsclient frames
(add-hook 'emacs-startup-hook #'kzar/setup-gui-frame)         ; initial non-daemon frame

;; Readable Unicode symbols (deferred to the first GUI frame).
(use-package unicode-fonts
  :commands unicode-fonts-setup
  :init (add-hook 'kzar/gui-frame-hook #'unicode-fonts-setup))

;; Magit mode
(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :custom ((magit-diff-refine-hunk t)
           (magit-revision-insert-related-refs-display-alist
            ;; Don't display "Follows:" and "Precedes:" for revisions, it's too
            ;; slow for large repositories.
            '((follows . nil) (precedes . nil)))))

;; Don't display tags, it's too slow for large repositories.
(with-eval-after-load 'magit-status
  (remove-hook 'magit-status-headers-hook #'magit-insert-tags-header))
(with-eval-after-load 'magit-refs
  (remove-hook 'magit-refs-sections-hook #'magit-insert-tags))

;; Languages

;; Fall back to JSON mode until the JSON5 tree-sitter grammar is installed.
(define-derived-mode json5-mode js-json-mode "JSON5"
  "Major mode for editing JSON5 without tree-sitter support.")

(use-package json5-ts-mode
  :commands json5-ts-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.json5\\'" . json5-mode)))

;; Tree-sitter
(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (add-to-list
   'treesit-auto-recipe-list
   (make-treesit-auto-recipe
    :lang 'json5
    :ts-mode 'json5-ts-mode
    :remap 'json5-mode
    :url "https://github.com/Joakker/tree-sitter-json5"
    :ext "\\.json5\\'"))
  (setq treesit-auto-langs
        '(bash c cpp css javascript json json5 python rust tsx typescript yaml))
  (global-treesit-auto-mode))
(setq treesit-font-lock-level 4)

;; Chromium-maintained language modes, vendored under lisp/chromium/.
(use-package gn-mode
  :ensure nil
  :mode ("\\.gni?\\'" . gn-mode))
(use-package mojom-mode
  :ensure nil
  :mode ("\\.\\(?:test-\\)?mojom\\'" . mojom-mode))
(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode))
(use-package bazel
  :mode ("\\.star\\'" . bazel-starlark-mode))

;; LSP
(defun kzar/eglot-clangd-command (_interactive project)
  ;; Use bundled clangd for Chromium, otherwise the system version.
  (let ((project-clangd
         (expand-file-name
          "third_party/llvm-build/Release+Asserts/bin/clangd"
          (project-root project))))
    (list (if (file-exists-p project-clangd)
              (file-local-name project-clangd)
            "clangd")
          ;; Throttle clangd background indexing, which can be slow for large
          ;; repos.
          (format "-j=%d" (max 1 (/ (num-processors) 2)))
          "--background-index-priority=background"
          ;; Recommended for Chromium; wrong headers are sometimes inserted.
          "--header-insertion=never"
          ;; Keep clangd output in the Eglot buffer quiet.
          "--log=error")))

(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode
          rust-ts-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  :custom ((eglot-autoshutdown t)
           ;; Don't log LSP events, too slow with clangd on large projects.
           (eglot-events-buffer-config '(:size 0)))
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)
                 . kzar/eglot-clangd-command)))

(use-package eldoc-box
  :hook (eglot-managed-mode .
         (lambda () (eldoc-box-hover-mode (if (eglot-managed-p) 1 -1)))))
(use-package breadcrumb
  :hook (eglot-managed-mode .
         (lambda () (breadcrumb-local-mode (if (eglot-managed-p) 1 -1)))))

;; Clojure / ClojureScript
(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.cljs\\'" . clojure-mode)))

;; JavaScript
(use-package js2-mode :commands js2-indent-bounce)
(setq js-indent-level 2)
(dolist (ext '("\\.cjs\\'" "\\.mjs\\'"))
  (add-to-list 'auto-mode-alist (cons ext #'js-mode)))

;; Keep the nice indent-bounce feature from js2-mode.
(dolist (hook '(js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook
   hook
   (lambda ()
     (electric-indent-local-mode -1)
     (keymap-local-set "<tab>" #'js2-indent-bounce))))

(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 2
        c-ts-mode-indent-style 'k&r))

;; C-c o jumps between sibling files (e.g. C/C++ header <-> source).
(setq find-sibling-rules
      '(("\\([^/]+\\)\\.\\(?:c\\|cc\\|cpp\\|cxx\\)\\'" "\\1.h" "\\1.hh" "\\1.hpp" "\\1.hxx")
        ("\\([^/]+\\)\\.\\(?:h\\|hh\\|hpp\\|hxx\\)\\'" "\\1.c" "\\1.cc" "\\1.cpp" "\\1.cxx")))
(keymap-global-set "C-c o" #'find-sibling-file)

;; Python
(setq python-indent-offset 2)
(add-to-list 'auto-mode-alist
             '("/\\(?:DEPS\\|WATCHLISTS\\)\\'" . python-mode))

;; Stop inferior shells from echoing input twice.
(defun echo-false-comint ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook #'echo-false-comint)

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command
        (cond ((executable-find "pandoc")       "pandoc -f gfm -t html")
              ((executable-find "cmark-gfm")     "cmark-gfm -e table")
              ((executable-find "multimarkdown") "multimarkdown")
              (t markdown-command))))

;; Web templates / HTML / PHP / Vue, etc.
(use-package web-mode
  :mode (("\\.phtml\\'"    . web-mode)
         ("\\.pt\\'"       . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.php\\'"      . web-mode)
         ("\\.erb\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'"   . web-mode)
         ("\\.html?\\'"    . web-mode)
         ("\\.tmpl\\'"     . web-mode)
         ("\\.vue\\'"      . web-mode))
  :hook (web-mode . (lambda () (keymap-local-set "RET" #'newline-and-indent)))
  :custom ((web-mode-display-table nil)
           (web-mode-code-indent-offset 2)
           (web-mode-markup-indent-offset 2))
  :config
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "blue")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "brown")
  (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "black")
  (set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "black")
  (set-face-attribute 'web-mode-doctype-face nil :foreground "purple")
  (set-face-attribute 'web-mode-function-name-face nil :foreground "blue")
  (set-face-attribute 'web-mode-function-call-face nil :foreground "black"))

;; CSS
(setq css-indent-offset 2)

;; org-mode
(use-package org
  :ensure nil
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda))
  :hook (org-mode . visual-line-mode)
  :init
  (setq org-log-done t
        org-agenda-files '("~/Davebox/todo-org/todo.org")
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                            (sequence "WAITING(w@/!)" "CANCELLED(c@/!)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "black" :weight bold))
                                 ("NEXT" . (:foreground "red" :weight bold))
                                 ("WAITING" . (:foreground "orange" :weight bold))
                                 ("DONE" . (:foreground "forest green" :weight bold))
                                 ("CANCELLED" . (:foreground "forest green" :weight bold)))
        org-log-reschedule 'time
        org-todo-repeat-to-state t
        calendar-week-start-day 1
        org-log-state-notes-into-drawer t
        org-archive-subtree-save-file-p t
        org-startup-indented t
        org-export-backends '(html md)))

(use-package ol-notmuch
  :after notmuch)

;; IRC
(setq rcirc-default-nick "kzar"
      rcirc-default-full-name "Dave Vandyke"
      rcirc-server-alist
      `(("irc.libera.chat" :port 6697 :encryption tls :password "")
        ("irc.oftc.net" :port 6697 :encryption tls :password "")))

(with-eval-after-load 'rcirc
  (setq rcirc-authinfo
        (condition-case nil
            (when-let* ((libera-password
                         (auth-source-pick-first-password
                          :host "irc.libera.chat" :user "kzar"))
                        (oftc-password
                         (auth-source-pick-first-password
                          :host "irc.oftc.net" :user "kzar")))
              `(("libera" nickserv "kzar"
                 ,(format "%s %s" "kzar" libera-password))
                ("oftc" nickserv "kzar"
                 ,(format "%s %s" oftc-password "kzar"))))
          ((error quit) nil)))
  (rcirc-track-minor-mode 1))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (jinx-mode 1)
            (setq rcirc-ignore-buffer-activity-flag t)))

;; Tramp
(setq tramp-default-method "ssh"
      enable-remote-dir-locals t)

;; Manually-invoked tools.
(use-package php-mode :defer t)
(use-package notmuch :commands notmuch)

;; Misc keybindings.
(keymap-global-set "C-c SPC" #'kzar/indent-rectangle)
(keymap-global-set "C-c d" #'duplicate-dwim)
(keymap-global-set "C-c s" #'code-search/search)

;; Garbage collection. (Set high threshold while active, then collect on idle.)
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :custom ((gcmh-idle-delay 'auto)
           (gcmh-auto-idle-delay-factor 10)
           (gcmh-high-cons-threshold (* 64 1024 1024))))

;; Load host-specific settings from hosts/.
(let* ((short-host (downcase (car (split-string (system-name) "\\."))))
       (local-conf (expand-file-name (format "%s.el" short-host) "~/.emacs.d/hosts")))
  (cond ((file-exists-p local-conf)
         (load-file local-conf))
        ((message "%s doesn't exist or I'd load it." local-conf))))
