;; Settings for my desktop computer

;; Fix Unicode symbols
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Make highlighted text "yankable"
(setq x-select-enable-primary t)

;; Tweak the font size
(set-face-attribute 'default nil :height 110)

;; Get the toolbar icons back
(setq tool-bar-style 'both)

;; Ensure the right browser is used to open links.
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program "~/firefox/dev/firefox")

;; Setup email
(load "~/work/personal/davemail/davemail.el")
