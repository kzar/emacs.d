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