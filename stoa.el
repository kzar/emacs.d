; Settings for my Thinkpad T470


; Make highlighted text "yankable"
(setq x-select-enable-primary t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

; Hide frame toolbars
(defun kzar/setup-frame (_)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ; Display Unicode symbols with a nice colour font.
  ; Note: Requires a very recent version (27/28) of Emacs!
  ;
  ; FIXME - Will this cause a memory leak prepending the font each time
  ;         a frame opens?
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))

(kzar/setup-frame nil)
(add-to-list 'after-make-frame-functions #'kzar/setup-frame)

(set-face-attribute 'default nil :height 110)

(load "~/work/personal/davemail/davemail.el")
