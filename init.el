
;; Uncomment to automatically download packages
;; (require 'use-package-ensure)
;; (setq use-package-always-ensure t)

;; Setup paths
;; (add-to-list 'load-path "~/.emacs.d/misc_lisp")
;; (add-to-list 'load-path "~/.emacs.d/")
(setq exec-path (append exec-path '("/home/sandy/android/android-sdk/platform-tools/")))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             'APPEND)

(package-initialize)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))
(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(load-user-file "core_config.el")
(load-user-file "helpers.el")
(load-user-file "keyboard-layout-adjustments.el")

;; (load-user-file "show-ifdefs.el")

(use-package glsl-mode
  :mode "\\.kv$")

(use-package helm-projectile)

(use-package helm
  :demand
  :bind
  (("M-x" . helm-M-x)
   ("C-c h o" . helm-occur)
   ("C-c h g" . helm-do-grep)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-h b" . helm-descbinds)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action)
   ("C-o" . helm-select-action)
   ("C-e" . helm-previous-line)
   ("C-h" . helm-find-files-up-one-level))
  :init
  (global-unset-key (kbd "C-x c"))
  :config
  (require 'helm-config)
  (require 'helm-projectile)
  (helm-projectile-on)
  (setq projectile-project-search-path '("~/devel"))
  (helm-mode)
  )

(use-package projectile
  :demand
  :init
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil)
  :config
  (projectile-mode))

(use-package undo-tree
  :demand
  :config
  (global-undo-tree-mode))

(use-package helm-gtags)
(setq helm-gtags-ignore-case t)
(setq helm-gtags-auto-update t)
(add-hook 'c-mode-hook (lambda () (message "c-mode hook ran")))
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'cc-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
;; todo: use-package method fails with autoloading failing to define helm-gtags?
;; (use-package helm-gtags
;;   :demand
;;   :hook
;;   (c-mode . helm-gtags-mode)
;;   (c++-mode . helm-gtags-mode)
;;   :init
;;   (setq helm-gtags-ignore-case t)
;;   (setq helm-gtags-auto-update t)
;;   )

(add-to-list 'load-path "~/.emacs.d/lisp")
(use-package preprocessor-collect)

(use-package evil
  :demand
  :hook
  (with-editor-mode . evil-insert-state)
  :bind (:map evil-normal-state-map
              ("m" . nil)
              ("mq" . show-ifdefs)
              ("md" . nil)
              ("mdp" . preprocessor-collect-toggle-overlay-ifdefs)
              ("mdc" . preprocessor-collect-overlay-clear)
         :map evil-motion-state-map
              ; General
              ("mx" . helm-M-x)
              ("ms" . helm-semantic-or-imenu)
              ("mt" . helm-command-prefix)
              ("mv" . magit-status)
              ("mf" . helm-find-files)
              ("mb" . helm-mini)
              ("mr" . helm-resume)
              ("mu" . undo-tree-visualize)
              ("mo" . helm-swoop)
              ("mp" . projectile-command-map)
              ("m/" . swiper-helm)
              ("me" . nil)
              ("mee" . eval-region)
              ; Magit
              ("mg"  . nil)
              ("mgb" . magit-blame)
              ; Flycheck
              ("mc" . nil)
              ;; ("mcc" . flycheck-buffer)
              ;; ("mct" . flycheck-mode)
              ;; ("mch" . helm-flycheck)
              ;; ("mce" . flycheck-list-errors)
              ;; ("mcn" . flycheck-next-error)
              ;; ("mcp" . flycheck-previous-error)
              ;; ("mcf" . flycheck-first-error)
              ("mct" . flymake-mode)
              ("mcn" . flymake-goto-next-error)
              ("mcp" . flymake-goto-prev-error)
              ; Gtags
              ("mi" . nil)
              ("mit" . helm-gtags-find-tag)
              ("mis" . helm-gtags-find-symbol)
              ("mig" . helm-gtags-find-pattern)
              ("mip" . helm-gtags-find-pattern)
              ("mir" . helm-gtags-find-rtag)
              ("miq" . helm-gtags-pop-stack)
              ("mih" . helm-gtags-find-tag-from-here)
              ("mi." . helm-gtags-find-tag-from-here)
              ("miS" . helm-gtags-select)
              ("mia" . helm-gtags-select)
              ("mif" . helm-gtags-tags-in-this-function)
              ("miu" . helm-gtags-show-stack)
              ; Code browsing (intended for eglot, but generic)
              ("my" . nil)
              ("myt" . xref-find-definitions)
              ("myr" . xref-find-references)
              ("myq" . xref-pop-marker-stack)
              ("mya" . xref-find-apropos)
              ("myh" . eglot-help-at-point)
              ("myc" . eglot-code-actions)
              ("my!" . eglot-rename)
         )
  :init
  ;; (setq evil-want-C-i-jump nil)  ; Avoid consuming tab key in terminal
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  ;; TODO: try disabling these and reconfiguring with evil-magit
  (add-to-list 'evil-insert-state-modes 'git-commit)
  ;; (add-to-list 'evil-emacs-state-modes 'magit-popup-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'magit-revision-mode)
  ;; (add-to-list 'evil-emacs-state-modes 'magit-mode)
  (evil-mode 1)
  (to-colemak)
  (evil-ex-define-cmd "W" 'save-buffer)
  )

(use-package magit)

(defun my-colemak-rotation (_mode mode-keymaps &rest _rest)
  (message "my-colemak-rotation called for mode %s keymaps %s" _mode mode-keymaps)
  (evil-collection-translate-key 'normal mode-keymaps
                                 "n" "j"
                                 "e" "k"
                                 "i" "l"
                                 "j" "e"
                                 "k" "n"
                                 "l" "i"))
(add-hook 'evil-collection-setup-hook #'my-colemak-rotation)
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )


;; (use-package evil-magit
;;   :demand
;;   :config
;;   (evil-define-key 'visual magit-mode-map "n" 'evil-next-visual-line)
;;   (evil-define-key 'visual magit-mode-map "e" 'evil-previous-visual-line)
;;   (evil-define-key evil-magit-state magit-mode-map "n" 'magit-next-line)
;;   (evil-define-key evil-magit-state magit-mode-map "e" 'magit-previous-line)
;;   (evil-define-key evil-magit-state magit-mode-map (kbd "C-n") 'magit-section-forward)
;;   (evil-define-key evil-magit-state magit-mode-map (kbd "C-e") 'magit-section-backward)
;;   (evil-define-key evil-magit-state magit-mode-map "p" 'magit-section-backward)
;;   (evil-define-key evil-magit-state magit-mode-map (kbd "C-p") 'magit-section-backward)
;;   (evil-define-key evil-magit-state magit-mode-map "k" 'evil-search-next)
;;   (evil-define-key evil-magit-state magit-mode-map "K" 'evil-search-previous))

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  :config
  (sml/setup))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(use-package org
  :bind
  ("M-n" . org-metadown)
  ("M-i" . org-metaup)
  ("M-i" . org-metaright)
  ("M-h" . org-metaleft)
  ("M-I" . org-shiftright)
  ("M-H" . org-shiftleft)
  ("M-n" . org-metadown)
  ("M-i" . org-metaup)
  ("M-i" . org-metaright)
  ("M-h" . org-metaleft)
  ("M-I" . org-shiftright)
  ("M-H" . org-shiftleft)
  ("C-c a" . org-agenda)
  )

(use-package eglot
  :demand
  :hook
  (c-mode . eglot-ensure)
  (cc-mode . eglot-ensure)
  (c++-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  :init
  (setq eglot-ignored-server-capabilites '(:documentHighlightProvider
                                           :hoverProvider))
  ;; Increasing these variables helps lsp-mode, perhaps eglot too?
  ;; todo: check this
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (defun project-root (project)
    "Function required by eglot but not present in project.el? See eglot issue #492"
    (car (project-roots project)))
  :config
  (add-to-list 'eglot-server-programs '((cc-mode c-mode c++-mode) . ("clangd" "--all-scopes-completion" "--background-index")))
  )

(use-package nyan-mode)
(nyan-mode 1)

(semantic-mode 1)

(defun my-projectile-project-find-function (dir)
  (message "Project of dir %s is %s" dir (projectile-project-root dir))
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))
(use-package project
  :demand
  :config
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

(use-package helm-xref)

(define-key global-map "\M-Q" 'unfill-paragraph)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(run-at-time nil 60 'recentf-save-list)

(load-user-file "python-for-android.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(py-set-fill-column-p t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
