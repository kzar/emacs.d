; Get Emacs Client working (kinda, sorta)
(require 'server)
(unless (server-running-p)
  (server-start))
(unless (package-installed-p 'mac-pseudo-daemon)
  (package-refresh-contents)
  (package-install 'mac-pseudo-daemon))
(require 'mac-pseudo-daemon)
(mac-pseudo-daemon-mode 1)

; Get hot keys like M-w working using Option key.
(setq ns-command-modifier 'super
      ns-option-modifier 'meta
      ns-right-option-modifier 'none)
; (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

; Tweak the font size.
(set-face-attribute 'default nil :height 125)

; Get Hunspell working.
(setenv "DICPATH" (expand-file-name "~/Library/Spelling"))
