(setq user-full-name "Dave Barker")
(setq user-mail-address "kzar@kzar.co.uk")
(load "~/.emacs.d/my-helpers.el")
(load "~/.emacs.d/my-secrets.el")

(require 'cl)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(defvar kzar/packages '(cider
                        clojure-mode
                        clojure-test-mode
                        coffee-mode
                        dash
                        epl
                        font-utils
                        ir-black-theme
                        js2-mode
                        list-utils
                        magit
                        markdown-mode
                        nrepl
                        paredit
                        pcache
                        persistent-soft
                        php-mode
                        pkg-info
                        puppet-mode
                        rcirc-notify
                        rvm
                        ucs-utils
                        unicode-fonts
                        web-mode
                        yaml-mode
                        yasnippet))

(defun abedra/packages-installed-p (packages)
  (loop for pkg in packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (abedra/packages-installed-p kzar/packages)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg kzar/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

(setq ;inhibit-splash-screen t
      ;initial-scratch-message t
      initial-major-mode 'org-mode)

(prefer-coding-system 'utf-8)

(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; Enable column numbering
(setq column-number-mode t)

; Highlight matching parenthesis green
(show-paren-mode 1)
(setq show-paren-delay-0)
(set-face-background 'show-paren-match "#99FF00")

; Set default code indent to 2 spaces
(setq tab-width 2
      indent-tabs-mode nil)

; Replace zap-to-char with custom zap-up-to-char function
(global-set-key "\M-z" 'zap-up-to-char)

; Magit mode
(global-set-key (kbd "C-c g") 'magit-status)
(setq magit-diff-refine-hunk t)
(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))

; org-mode
(add-to-list `auto-mode-alist `("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE")
                          (sequence "WAITING(w@/!)" "CANCELLED(c@/!)"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))
                               ("WAITING" . (:foreground "dark orange" :weight bold))
                               ("CANCELLED" (:foreground "green" :weight bold))))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING") ("CANCELLED"))
              ("DONE" ("WAITING") ("CANCELLED")))))
(setq org-startup-indented t)

; Tramp
(setq tramp-default-method "ssh")

; Setup buffer switching
(iswitchb-mode)
(setq iswitchb-default-method 'samewindow)

; Add to the executable path for executable-find
(add-to-list `exec-path "/usr/local/bin")
(add-to-list `exec-path "/opt/local/bin")

; Frame width
(defun fix-frame-width ()
  (interactive)
  (set-frame-width (selected-frame 80)))
(add-to-list 'default-frame-alist '(width . 80))

; Highlight tabs and trailing whitespace red
(setq whitespace-style '(face trailing tabs))
(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))
(global-whitespace-mode)

; Fix flyspell
(setq-default ispell-program-name "aspell")
(setq ispell-program-name "aspell")
(setq flyspell-issue-welcome-flag nil)
(setq flyspell-default-dictionary "british"
      ispell-local-dictionary "british"
      ispell-dictionary "british")

; IRC
(require 'tls)
(setq rcirc-server-alist `(("irc.freenode.net" :port 31337 :nick "kzar"
                            :password ,(format "%s/irc.freenode.net:%s" znc-user znc-password)
                            :encryption tls)
                           ("irc.mozilla.org" :port 31337 :nick "kzar"
                            :password ,(format "%s/irc.mozilla.org:%s" znc-user znc-password)
                            :encryption tls)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(setq rcirc-notify-timeout 0)
(rcirc-notify-add-hooks)

;; Clojure
(require 'clojure-mode)
(setq clojure-mode-use-backtracking-indent t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setf js2-bounce-indent-p t)
(setq-default js2-basic-offset 2)

;; Snipplets
(require 'yasnippet)

; CoffeeScript
(add-to-list `auto-mode-alist `("\\.coffee$" . coffee-mode))
(add-to-list `auto-mode-alist `("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
          '(lambda ()
             (setq tab-width 2)))

; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Python
(setq python-indent 2)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))
; Ruby
(rvm-use-default)
(defun echo-false-comint ()
  (setq comint-process-echoes t))
(add-hook 'comint-mode-hook 'echo-false-comint)
(setq ruby-insert-encoding-magic-comment nil)
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

; ansi-term
(setenv "PROMPT_COMMAND")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq term-suppress-hard-newline t)

; Clojurescript
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))
(defun browser-repl (lisp-path)
  (interactive "DCLJS project path: ")
  (cd lisp-path)
  (run-lisp "/usr/local/bin/lein trampoline cljsbuild repl-listen"))

; Markdown and reStructuredText
(add-to-list `auto-mode-alist `("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'rst-mode-hook (lambda () (flyspell-mode 1)))

; Puppet
(add-to-list `auto-mode-alist `("\\.pp$" . puppet-mode))

; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl?\\'" . web-mode))
(setq web-mode-display-table nil)
(setq-default indent-tabs-mode nil)
(set-face-attribute 'web-mode-html-tag-face nil :foreground "blue")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "brown")
(set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "black")
(set-face-attribute 'web-mode-html-attr-equal-face nil :foreground "black")
(set-face-attribute 'web-mode-doctype-face nil :foreground "purple")
(set-face-attribute 'web-mode-function-name-face nil :foreground "blue")
(set-face-attribute 'web-mode-function-call-face nil :foreground "black")
(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline-and-indent)
            (whitespace-mode 0)))
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

; Force Emacs to allow us to edit files that are mistakenly marked readonly
; (Sometimes an issue when mounting over NFS)
; http://kzar.co.uk/blog/2013/12/09/disable-emacs-read-only-warning-tramp-nfs/
(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p (buffer-file-name))
              (set (make-local-variable 'inhibit-read-only) t)
              (fset (make-local-variable 'file-writable-p) (lambda (filename) t))
              (set (make-local-variable 'buffer-read-only) nil))))

; Load machine specific settings
; http://emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/#comment-36295
(let ((local-conf-name (format "~/.emacs.d/%s.el" system-name)))
  (cond ((file-exists-p local-conf-name)
         (load-file local-conf-name))
        ((message "%s doesn’t exist or I’d load it." local-conf-name))))
