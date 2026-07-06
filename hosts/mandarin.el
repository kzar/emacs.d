;;; -*- lexical-binding: t; -*-

; Get Emacs Client working (kinda, sorta)
(require 'server)
(unless (server-running-p)
  (server-start))
(use-package mac-pseudo-daemon
  :config (mac-pseudo-daemon-mode 1))

; Get hot keys like M-w working using Option key.
(setq ns-command-modifier 'super
      ns-option-modifier 'meta
      ns-right-option-modifier 'none)

; Tweak the font size.
(set-face-attribute 'default nil :height 125)

;; Use my DuckDuckGo org file instead of my personal org files
(setq org-agenda-files '("~/Duckbox/ddg-org"))
