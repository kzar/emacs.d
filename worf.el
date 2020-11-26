; Settings for my desktop computer

; Fix Unicode symbols
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

; Make highlighted text "yankable"
(setq x-select-enable-primary t)

;; desktop-mode appears to have a bug when used in combination with Emacs
;; server. As a workaround, we force loading the Desktop state, even when
;; it is considered locked.
(setq desktop-load-locked-desktop t)

;; Use desktop-mode to maintain open buffers. This means I can shut down the
;; computer more often to save power.
(desktop-save-mode 1)
(desktop-read)

(defun first-frame-opened (frame)
  ;; Hide frame toolbars
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; Tidy up the hook.
  (remove-hook 'after-make-frame-functions 'first-frame-opened))

(add-hook 'after-make-frame-functions 'first-frame-opened)

; Tweak the font size
(set-face-attribute 'default nil :height 105)
