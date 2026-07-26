;;;  -*- lexical-binding: t; -*-

;; Raise the GC threshold for a fast startup, gcmh takes over runtime GC from
;; `emacs-startup-hook' onward.
(setq gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6)
