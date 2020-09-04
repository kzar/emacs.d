(let ((debian-startup "/usr/share/emacs/site-lisp/debian-startup.el"))
  (when (file-exists-p debian-startup)
    (load-file debian-startup)))

(setq user-full-name "Dave Vandyke")
(setq user-mail-address "kzar@kzar.co.uk")
(load "~/.emacs.d/my-helpers.el")
(load "~/.emacs.d/my-secrets.el")

(setq custom-file "~/.emacs.d/my-custom.el")
(load custom-file)

(require 'cl)

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defvar kzar/packages '(ace-window
                        cider
                        clojure-mode
                        coffee-mode
                        dash
                        epl
                        font-utils
                        gist
                        gnus-alias
                        ir-black-theme
                        js2-mode
                        list-utils
                        lsp-mode
                        magit
                        markdown-mode
                        paredit
                        pcache
                        persistent-soft
                        php-mode
                        pkg-info
                        puppet-mode
                        notmuch
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

(load "~/.emacs.d/lisp/org-notmuch.el")

(setq inhibit-splash-screen t
      initial-scratch-message ";; Hello Dave\n"
      initial-major-mode 'org-mode)

(prefer-coding-system 'utf-8)

; Make some special Unicode symbols more readable
(unicode-fonts-setup)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

; Enable column numbering
(setq column-number-mode t)

; Highlight matching parenthesis green
(show-paren-mode 1)
(setq show-paren-delay 0)
(set-face-background 'show-paren-match "#99FF00")

; Set default code indent to 2 spaces
(setq tab-width 2
      indent-tabs-mode nil)

; Replace zap-to-char with custom zap-up-to-char function
(global-set-key "\M-z" 'zap-up-to-char)

; Magit mode
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(setq magit-diff-refine-hunk t)
(add-hook 'git-commit-mode-hook (lambda () (flyspell-mode 1)))
(setq magit-last-seen-setup-instructions "2.1.0")
(setq magit-revert-buffers t)
(setq magit-push-always-verify nil)

; org-mode
(add-to-list `auto-mode-alist `("\\.org$" . org-mode))
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files '("~/Davebox/org"))
(setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "CODEREVIEW" "BLOCKED"
                                    "DONE")
                          (sequence "CANCELLED(c@/!)"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))
                               ("CODEREVIEW" . (:foreground "dark orange" :weight bold))
                                ("BLOCKED" . (:foreground "dark red" :weight bold))
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
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (python . t)
   (ruby . t)
   (js . t)))
(setq org-src-fontify-natively t)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(eval-after-load "org"
  (lambda ()
    (require 'ox-md nil t)
    (require 'ox-odt nil t)
    (require 'ox-publish nil t)
    (require 'org-notmuch)))

; IRC
(require 'tls)
(setq rcirc-default-nick "kzar")
(setq rcirc-default-full-name "Dave Vandyke")
(setq rcirc-authinfo
      `(("freenode" nickserv "kzar" ,(format "%s %s" "kzar" freenode-password))
        ("oftc" nickserv "kzar" ,(format "%s %s" oftc-password "kzar"))))
(setq rcirc-server-alist
      `(("irc.freenode.net" :port 7000 :encryption tls)
        ("irc.oftc.net" :port 6697 :encryption tls)))

(rcirc-track-minor-mode 1)
(add-hook 'rcirc-mode-hook
          (lambda ()
            (flyspell-mode 1)
            (setq rcirc-ignore-buffer-activity-flag t)))

; Tramp
(setq tramp-default-method "ssh")

; Setup buffer switching
(ido-mode)
(setq ido-create-new-buffer 'always
      ido-default-buffer-method  'selected-window
      ido-default-file-method 'selected-window
      ido-enable-flex-matching t)
(add-to-list `ido-ignore-files "~$")

(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-c g") 'ido-enter-magit-status)))

; Convenient window switching
(global-set-key (kbd "C-c w") 'ace-window)

; Add to the executable path for executable-find
(add-to-list `exec-path "/usr/local/bin")
(add-to-list `exec-path "/opt/local/bin")

; Highlight tabs and trailing whitespace (custom-set-faces defines colours)
(setq whitespace-style '(face trailing tabs lines-tail space-mark))
(setq whitespace-global-modes '(not rcirc-mode magit-mode))
(global-whitespace-mode)
; Display zero-width unicode characters as standard spaces so we don't miss them
(setq whitespace-display-mappings '((space-mark ?\x200B [? ])
                                    (space-mark ?\x200C [? ])
                                    (space-mark ?\x200D [? ])
                                    (space-mark ?\xFEFF [? ])))
; Add all unicode characters to tab regexp so we highlight them too
(setq whitespace-tab-regexp "\\([\t[:nonascii:]]\\)")

(setq rcirc-dim-nicks '("abpbot"))
(setq rcirc-bright-nicks '("palant" "fhd" "snoack"))

; https://www.emacswiki.org/emacs/rcircCloudToButt
(defun kzar/rcirc-tweak-messages (sender response)
  (while (re-search-forward "feel free to" nil t)
    (replace-match "please"))
  (while (re-search-forward "ASAP" nil t)
    (replace-match "at some point" t)))
(add-hook 'rcirc-markup-text-functions 'kzar/rcirc-tweak-messages)

; Flyspell
(setq ispell-program-name "hunspell"
      flyspell-issue-welcome-flag nil
      ispell-local-dictionary "en_GB"
      ispell-local-dictionary-alist
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))

;; Clojure
(require 'clojure-mode)
(setq nrepl-log-messages t)
(setq nrepl-hide-special-buffers t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cjs$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
(setq js2-highlight-level 3)
(setf js2-bounce-indent-p t)
(add-hook 'js2-mode-hook (lambda () (electric-indent-local-mode -1)))
(setq-default js2-basic-offset 2)
(setq js-indent-level 2)

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
(setq python-indent 2
      python-indent-offset 2)
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
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
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
            (local-set-key (kbd "RET") 'newline-and-indent)))
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)

; CSS Mode
(setq css-indent-offset 2)

; Manually indent a region of code, taking care of trailing whitespace
(defun indent-rectangle ()
  (interactive)
  (when (region-active-p)
    (save-excursion
      (let ((start (min (region-beginning) (region-end)))
            (end (max (region-beginning) (region-end)))
            (region-indent nil)
            (first-line-start 0)
            (last-line-start 0)
            (last-line-end 0))
        ; Record the start / end positions
        (goto-char end)
        (setq last-line-end (line-end-position))
        (goto-char start)
        (setq first-line-start (line-beginning-position))
        ; Figure out the base indentation for the selected region
        (while (< (point) last-line-end)
          (unless (= (line-end-position) (line-beginning-position))
            (setq region-indent (min (or region-indent (current-indentation))
                                     (current-indentation)))
            (setq last-line-start (line-beginning-position)))
          (forward-line))
        ; Use rectangle mark mode to select the base indentation and
        ; prompt the user to alter that region
        (push-mark first-line-start)
        (goto-char (+ last-line-start region-indent))
        (call-interactively 'string-rectangle)
        ; Clear any trailing whitespace
        (delete-trailing-whitespace (region-beginning) (region-end)))
      (pop-mark))))

(global-set-key (kbd "C-c SPC") 'indent-rectangle)

; C++
(require 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(lsp-register-client
  (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                   :major-modes '(c++-mode)
                   :remote? t
                   :server-id 'clangd-remote))

; Load machine specific settings
; http://emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/#comment-36295
(let ((local-conf-name (format "~/.emacs.d/%s.el" system-name)))
  (cond ((file-exists-p local-conf-name)
         (load-file local-conf-name))
        ((message "%s doesn't exist or I'd load it." local-conf-name))))
