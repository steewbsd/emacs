(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq read-process-output-max (* 1024 1024))

;;; load theme
(setq seoul256-background 233)
(load-theme 'seoul256 1)
;;; steal helm theme from seoul256
(load-theme 'monokai-pro-spectrum t)

(load-file "/usr/src/tools/tools/editing/freebsd.el")
;;; change default font
(setq default-frame-alist '((font . "Ac437 IBM VGA 8x16 23")))
;; Start in fullscreen mode
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter nil 'undecorated t)
;; Disable bars and scrollbars
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(setq inhibit-splash-screen t)
;;; save custom variables elsewhere
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)
;;; initial package load
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
										;(unless package-archive-contents
										;(package-refresh-contents))
;;; check for use-package, which we will use afterwards
(unless (package-installed-p 'use-package) 
  (package-install 'use-package))
;;; change default dashboard

										;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
										;(setq display-line-numbers 'relative)

(add-hook 'prog-mode-hook 'writeroom-mode)

(use-package 
  dashboard 
  :ensure t 
  :config (dashboard-setup-startup-hook))
;;; dired icons
(use-package 
  all-the-icons-dired 
  :ensure t 
  :hook (dired-mode . all-the-icons-dired-mode))
;;; use dired as a sidebar
(use-package 
  dired-sidebar 
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar)) 
  :ensure t 
  :commands (dired-sidebar-toggle-sidebar))

(use-package 
  doom-modeline 
  :ensure t 
  :init)
(doom-modeline-mode 1)
(use-package 
  helm 
  :ensure t 
  :init)
(helm-mode 1)
(use-package 
  lsp-mode 
  :ensure t 
  :hook (prog-mode . lsp-mode))
(use-package 
  swiper-helm 
  :ensure t)
(use-package 
  flycheck 
  :ensure t 
  :init (global-flycheck-mode))
(use-package 
  yasnippet 
  :hook (c-mode . yas-global-mode))
(use-package 
  smartparens 
  :hook (c-mode . smartparens-mode))
;;; company config
(use-package 
  company 
  :ensure t 
  :init (global-company-mode))
(setq company-idle-delay 0)
(add-to-list 'company-backends 'company-elisp)

(use-package 
  expand-region 
  :bind ("C-;" . er/expand-region) 
  :ensure t)

;; text size management
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; I-search with consult-line
										;(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-s") 'swiper-helm)
;; Replace M-x with smart mx (smex)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

;;(global-set-key (kbd "M-x") 'smex)
(pinentry-start)
										;(server-start)

(put 'upcase-region 'disabled nil)
