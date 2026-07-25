;;; -*- lexical-binding: t; -*-

;; Use a custom file, but make it optional and keep it out of the repo.
;; Note: Needs to happen before packages are loaded.
(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file t)

;; Load Debian site packages (where relevant) early.
(let ((debian-startup "/usr/share/emacs/site-lisp/debian-startup.el"))
  (when (file-exists-p debian-startup)
    (load-file debian-startup)))

;; Setup packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load local libraries.
(eval-and-compile
  (dolist (dir '("lisp" "lisp/rich-text"))
    (add-to-list 'load-path (expand-file-name dir user-emacs-directory))))
(require 'my-helpers)
(require 'rich-text)
(load "~/.emacs.d/my-secrets.el")

;; Set my username etc.
(setq user-full-name "Dave Vandyke"
      user-mail-address "kzar@kzar.co.uk")

;; Editor basics.
(setq inhibit-splash-screen t
      initial-scratch-message ";; Hello Dave\n"
      initial-major-mode 'org-mode)

(prefer-coding-system 'utf-8)

(dolist (dir '("/opt/homebrew/bin" "/usr/local/bin" "~/.cargo/bin"))
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

(defvar consult-fd-args)
(defun kzar/project-find-dir ()
  (interactive)
  (let ((consult-fd-args
         '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
           "--full-path --color=never --type=d")))
    (consult-fd)))

(with-eval-after-load 'project
  (keymap-set project-prefix-map "f" #'consult-fd)
  (keymap-set project-prefix-map "g" #'consult-ripgrep)
  (keymap-set project-prefix-map "d" #'kzar/project-find-dir)
  (keymap-set project-prefix-map "m" #'magit-project-status)
  (when-let ((e (assq 'project-find-file   project-switch-commands)))
    (setcar e #'consult-fd))
  (when-let ((e (assq 'project-find-regexp project-switch-commands)))
    (setcar e #'consult-ripgrep))
  (when-let ((e (assq 'project-find-dir    project-switch-commands)))
    (setcar e #'kzar/project-find-dir))
  (add-to-list 'project-switch-commands '(magit-project-status "Magit") t))

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
(setq display-fill-column-indicator-column t)
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Spell-checking
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-c C-M-i" . jinx-correct))
  :custom (jinx-languages "en_GB")
  :config
  (set-face-attribute 'jinx-misspelled nil
                      :underline '(:style wave :color "Red1")))

;; Fonts
(defvar kzar/gui-frame-hook nil
  "Hook run once in the first graphical frame, for font/face setup.")
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
  :custom (magit-diff-refine-hunk t))

;; Languages

;; Tree-sitter
(use-package treesit-auto
  :custom (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs
        '(bash c cpp css javascript json python rust tsx typescript yaml))
  (global-treesit-auto-mode))
(setq treesit-font-lock-level 4)

;; LSP
(use-package eglot
  :ensure nil
  :hook ((typescript-ts-mode tsx-ts-mode js-ts-mode
          rust-ts-mode c-ts-mode c++-ts-mode) . eglot-ensure)
  :custom (eglot-autoshutdown t))

(use-package eldoc-box
  :hook (eglot-managed-mode . eldoc-box-hover-mode))
(use-package breadcrumb
  :hook (prog-mode . breadcrumb-local-mode))

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
  :demand t
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
  :after org
  :demand t)

;; IRC
(setq rcirc-default-nick "kzar"
      rcirc-default-full-name "Dave Vandyke"
      rcirc-authinfo
      `(("libera" nickserv "kzar" ,(format "%s %s" "kzar" libera-password))
        ("oftc" nickserv "kzar" ,(format "%s %s" oftc-password "kzar")))
      rcirc-server-alist
      `(("irc.libera.chat" :port 6697 :encryption tls)
        ("irc.oftc.net" :port 6697 :encryption tls)))
(rcirc-track-minor-mode 1)
(add-hook 'rcirc-mode-hook
          (lambda ()
            (jinx-mode 1)
            (setq rcirc-ignore-buffer-activity-flag t)))

;; Tramp
(setq tramp-default-method "ssh")

;; Manually-invoked tools.
(use-package php-mode :defer t)
(use-package notmuch  :defer t)

;; Misc keybindings.
(keymap-global-set "C-c SPC" #'kzar/indent-rectangle)
(keymap-global-set "C-c d" #'duplicate-dwim)

;; Garbage collection. (Set high threshold while active, then collect on idle.)
(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :custom ((gcmh-idle-delay 'auto)
           (gcmh-auto-idle-delay-factor 10)
           (gcmh-high-cons-threshold (* 64 1024 1024))))

;; DuckDuckGo-specific rules.
(add-to-list 'safe-local-variable-values '(js-indent-level . 4))
(dir-locals-set-class-variables
 'duckduckgo-directory
 '((nil . ((js-indent-level . 4)))))
(dir-locals-set-directory-class "~/work/duckduckgo/" 'duckduckgo-directory)
(add-to-list 'auto-mode-alist '("/duckduckgo-privacy-extension/build/.*" .
                                fundamental-mode))

;; Load host-specific settings from hosts/.
(let* ((short-host (downcase (car (split-string (system-name) "\\."))))
       (local-conf (expand-file-name (format "%s.el" short-host) "~/.emacs.d/hosts")))
  (cond ((file-exists-p local-conf)
         (load-file local-conf))
        ((message "%s doesn't exist or I'd load it." local-conf))))
