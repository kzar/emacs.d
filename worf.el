; Settings for my desktop computer

; Fix Unicode symbols
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

; Make highlighted text "yankable"
(setq x-select-enable-primary t)

; Hide frame toolbars
(defun kzar/hide-frame-toolbars (_)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(kzar/hide-frame-toolbars nil)
(add-to-list 'after-make-frame-functions #'kzar/hide-frame-toolbars)

; Tweak the font size
(set-face-attribute 'default nil :height 105)
