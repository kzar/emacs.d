; Settings for my desktop computer

; Make highlighted text "yankable"
(setq x-select-enable-primary t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xdg-open")

; Hide frame toolbars
(defun kzar/hide-frame-toolbars (_)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(kzar/hide-frame-toolbars nil)
(add-to-list 'after-make-frame-functions #'kzar/hide-frame-toolbars)
