;; Emacs configuration specific to my Macbook Pro

; Setup the Mac keys
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-option-modifier nil)
(setq mac-command-modifier 'meta)
(global-set-key (kbd "<kp-delete>") 'delete-char)

; Fix the Mac path
(setenv "PATH" (concat "/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/X11/bin:" (getenv "PATH")))

; Play Skype message sound on rcirc-notify IRC notifications
(add-hook 'rcirc-notify-page-me-hooks
          (lambda (msg)
            (start-process "beep-process" nil "afplay"
                           "/Applications/Skype.app/Contents/Resources/Sounds/Message Received .m4a")))
