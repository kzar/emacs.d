; Settings for my new Thinkpad T450

; Play Skype message sound on rcirc-notify IRC notifications
(add-hook 'rcirc-notify-page-me-hooks
          (lambda (msg)
            (start-process "beep-process" nil "aplay"
                           "/usr/share/skype/sounds/ChatIncoming.wav")))

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
