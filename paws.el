; Settings for my old Thinkpad T420

; Play Skype message sound on rcirc-notify IRC notifications
(add-hook 'rcirc-notify-page-me-hooks
          (lambda (msg)
            (start-process "beep-process" nil "aplay"
                           "/usr/share/skype/sounds/ChatIncoming.wav")))

(defun current-date ()
  (replace-regexp-in-string "[\s\n]*$" ""
                            (shell-command-to-string "date +'%d/%b/%Y %H:%M:%S'")))

; Store a log of notifications in case I need to look back through them
(shell-command "touch" "~/.cache/irc-notifications.log")
(add-hook 'rcirc-notify-page-me-hooks
          (lambda (msg)
            (write-region (format "[%s] - %s\n" (current-date) msg)
                          nil "~/.cache/irc-notifications.log" 'append)))

; Fix Unicode symbols
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))
