(setq user-full-name "Dave Barker")
(setq user-mail-address "kzar@kzar.co.uk")
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
                        markdown-mode
                        nrepl
                        paredit
                        pcache
                        persistent-soft
                        php-mode
                        pkg-info
                        puppet-mode
                        rvm
                        ucs-utils
                        unicode-fonts
                        web-mode
                        writegood-mode
                        yaml-mode
                        yasnippet
                        zone-matrix))

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

(show-paren-mode 1)
(setq show-paren-delay-0)
(set-face-background 'show-paren-match "#99FF00")

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq tab-width 2
      indent-tabs-mode nil)

(add-to-list `auto-mode-alist `("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/org"))
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "WAITING" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))
                               ("WAITING" . (:foreground "dark orange" :weight bold))))
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))
(add-hook 'org-mode-hook (lambda () (writegood-mode 1)))

(setq tramp-default-method "ssh")

; Setup buffer switching
(iswitchb-mode)
(setq iswitchb-default-method 'samewindow)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/epg/")
;(require 'epa-setup)

; Setup the Mac keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "<kp-delete>") 'delete-char)

; Fix the Mac path
(setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/X11/bin:" (getenv "PATH")))

; Add to the executable path for executable-find
(add-to-list `exec-path "/usr/local/bin")
(add-to-list `exec-path "/opt/local/bin")

; Fix flyspell
(setq-default ispell-program-name "aspell")
(setq ispell-program-name "aspell")
(setq flyspell-issue-welcome-flag nil)

; IRC
(require 'tls)
(setq rcirc-server-alist '(("irc.freenode.net" :port 31337 :nick "kzar"
                            :password (format "%s/irc.mozilla.org:%s" znc-user znc-password)
                            :encryption tls)
                           ("irc.mozilla.org" :port 31337 :nick "kzar"
                            :password (format "%s/irc.mozilla.org:%s" znc-user znc-password)
                            :encryption tls)))
(add-hook 'rcirc-mode-hook (lambda () (flyspell-mode 1)))
(load "~/.emacs.d/lisp-personal/rcirc-notify.el")

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; clojure-mode
;(load "~/clojure-mode.el")
;(require 'clojure-mode)
(setq clojure-mode-use-backtracking-indent t)

;; Javascript
;(load "~/.emacs.d/lisp/js2-mode.elc")
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setf js2-bounce-indent-p t)
(setq-default js2-basic-offset 2)

;; Snipplets
(require 'yasnippet)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;(when
;    (load
;     (expand-file-name "~/.emacs.d/elpa/package.el"))
;  (package-initialize))

;; Add auto-complete mode
;(add-to-list 'load-path "~/.emacs.d/")
;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;(ac-config-default)

; Use RVM to get correct ruby versions
;(load "~/.emacs.d/lisp/rvm.el")
(rvm-use-default)

; Turn off echo for ruby "irb" REPL
(defun echo-false-comint ()
(setq comint-process-echoes t))
(add-hook  'comint-mode-hook  'echo-false-comint)

; Stop Ruby mode inserting encoding everywhere
(setq ruby-insert-encoding-magic-comment nil)

;; Clojure repl highlighting
;(load "~/clojure-test-mode.el")
;(load "~/durendal.el")
;(load "~/init-clojure.el")

; Frame width
(defun fix-frame-width ()
  (interactive)
  (set-frame-width (selected-frame 80)))
(add-to-list 'default-frame-alist '(width . 80))

; Highlight tabs and trailing whitespace
(setq whitespace-style '(face trailing tabs))
(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))
;(add-hook 'prog-mode-hook 'whitespace-mode)
;(add-hook 'markdown-mode-hook 'whitespace-mode)
(global-whitespace-mode)


; Coffee-script mode
;(load "~/.emacs.d/lisp/coffee-mode.el")
(add-to-list `auto-mode-alist `("\\.coffee$" . coffee-mode))
(add-to-list `auto-mode-alist `("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
          '(lambda ()
             ;(whitespace-mode)
             (setq tab-width 2)))


; Enable column numbering
(setq column-number-mode t)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(defun describe-eol ()
  (interactive)
  (let ((eol-type (coding-system-eol-type buffer-file-coding-system)))
    (when (vectorp eol-type)
      (setq eol-type (coding-system-eol-type (aref eol-type 0))))
    (message "Line endings are of type: %s"
             (case eol-type
               (0 "Unix") (1 "DOS") (2 "Mac") (t "Unknown")))))

;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

; Set up ansi-term with colours as the shell of choice
(setenv "PROMPT_COMMAND")
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;(setq slime-net-coding-system 'utf-8-unix)

; Set up highlighting and brower-repl for clojurescript
(setq auto-mode-alist (cons '("\\.cljs" . clojure-mode) auto-mode-alist))

(defun browser-repl (lisp-path)
  (interactive "DCLJS project path: ")
  (cd lisp-path)
  (run-lisp "/usr/local/bin/lein trampoline cljsbuild repl-listen"))

; Get .erb files loading as html
;(add-to-list `auto-mode-alist `("\\.erb$" . html-mode))
; Get .twig files loading as html
;(add-to-list `auto-mode-alist `("\\.twig$" . html-mode))
; Get .mustache files loading as html
;(add-to-list `auto-mode-alist `("\\.mustache$" . html-mode))
; Stop html-mode using tabs
;(setq-default indent-tabs-mode nil)

(add-to-list `auto-mode-alist `("\\.md$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))

(add-hook 'rst-mode-hook (lambda () (flyspell-mode 1)))


;(load-file "~/.emacs.d/lisp/puppet-mode.el")
(add-to-list `auto-mode-alist `("\\.pp$" . puppet-mode))

; Set up web-mode for HTML etc
;(load "~/.emacs.d/lisp/web-mode.el")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl?\\'" . web-mode))
;(setq web-mode-whitespaces-regexp "^[\t]+\\|[\t ]+$")
;(setq web-mode-enable-whitespaces t)
(setq web-mode-display-table nil)
;(set-face-attribute 'web-mode-whitespace-face nil :background "red")
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

(defun zap-up-to-char (char)
  (interactive "cCharacter to delete up to: ")
  (zap-to-char 1 char)
  (insert char)
  (backward-char))
(global-set-key "\M-z" 'zap-up-to-char)

(add-hook 'find-file-hook
          (lambda ()
            (when (file-remote-p (buffer-file-name))
              (set (make-local-variable 'inhibit-read-only) t)
              (fset (make-local-variable 'file-writable-p) (lambda (filename) t))
              (set (make-local-variable 'buffer-read-only) nil))))
