;;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum)
(setq native-comp-deferred-compilation nil)
(setq read-process-output-max (* 1024 1024))

(setq monokai-background "#171717")
(load-theme 'monokai t)
(setq default-frame-alist
	  '((font . "Ac437 IBM VGA 8x16 23")))
(setq inhibit-splash-screen t)

(setq custom-file
	  (concat user-emacs-directory "custom.el"))
(load-file custom-file)

;; MELPA
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

;;; requires
(dolist (pkg
		 '(no-littering
		   all-the-icons-dired
		   dired-sidebar
		   doom-modeline
		   helm
		   swiper-helm
		   flycheck
		   yasnippet
		   smartparens
		   company
		   expand-region))
  (require pkg))

;;; startup
(global-company-mode  t)
(global-flycheck-mode t)
(doom-modeline-mode   t)
(helm-mode            t)
(tool-bar-mode        0)
(menu-bar-mode        0)
(pinentry-start)

;;; hooks
;; function definitions
(defun hl-asterisk ()
  "Force highlight asterisks in c-mode-hook."
  (font-lock-add-keywords nil '(("[*]" 0 font-lock-keyword-face t))))
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; hooks
(add-hook 'org-mode-hook        #'org-html-themify-mode)
(add-hook 'prog-mode-hook       #'yas-minor-mode)
(add-hook 'prog-mode-hook       #'smartparens-mode)
(add-hook 'c-mode-hook          #'writeroom-mode)
(add-hook 'c-mode-hook          #'hl-asterisk)

;;; global-set-key
(global-set-key (kbd "C-=")     #'text-scale-increase)
(global-set-key (kbd "C--")     #'text-scale-decrease)
(global-set-key (kbd "C-s")     #'swiper-helm)
(global-set-key (kbd "M-x")     #'helm-M-x)
(global-set-key (kbd "C-:")     #'er/expand-region)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(global-set-key (kbd "C-x C-n") #'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x w")   #'writeroom-mode)
