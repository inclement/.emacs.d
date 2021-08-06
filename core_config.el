
;; Make emacs not look terrible
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t)

;; Basic stuff that we always want:
(show-paren-mode 1)
(column-number-mode 1)

(setq-default fill-column 90)

;; Smooth scrolling when moving cursor
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
; Smooth scrolling when moving cursor
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Enable narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Make compilation buffers scroll automatically with output
(setf compilation-scroll-output t)

;; Do tabs right
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)
;; (setq indent-line-function nil)

(setq c-default-style "linux")
(setq c-basic-offset 4)

;; Do backups sensibly
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Clear trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Font config
;; (set-face-attribute 'default nil :height 100)
