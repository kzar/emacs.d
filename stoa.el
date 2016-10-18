; Settings for my new Thinkpad T450

; Fix Unicode symbols
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

; Make highlighted text "yankable"
(setq x-select-enable-primary t)

; Configure outgoing mail
(setq send-mail-function 'sendmail-send-it)
(setq sendmail-program "/usr/bin/msmtp")

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
