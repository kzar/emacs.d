;;; -*- lexical-binding: t; -*-

(add-hook 'kzar/gui-frame-hook #'kzar/setup-linux-fonts)

;; Ensure the right browser is used to open links.
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program "~/firefox/dev/firefox")

;; Setup email
(load "~/work/personal/davemail/davemail.el")
