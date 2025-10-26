;;; -*- lexical-binding: t; -*-
;;; Code:

;; Prevent initial white flash on startup
(setq gc-cons-threshold (* 50 1000 1000))
;;(setq native-comp-deferred-compilation nil)
(setq read-process-output-max (* 1024 1024))
(setq frame-background-mode 'light)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq default-frame-alist
      '((fullscreen . maximized)
        ;; (font . "Operator Mono 15")
        ))

(setq custom-file (make-temp-file "emacs-custom"))
(setq org-agenda-files (list "~/things/org/agenda.org"))
(if (file-exists-p custom-file)
    (load-file custom-file))

(setq-default indent-tabs-mode nil)
(setq auth-source
      (concat user-emacs-directory "authinfo.gpg"))
;; MELPA
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/"))

;; requires
(dolist (pkg
         '(
           corfu
           corfu-terminal
           doom-themes
           expand-region
           marginalia
           meow
           no-littering
           orderless
           pinentry
           rainbow-delimiters
           rustic
           smartparens
           vertico
           writeroom-mode
           ))
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; startup
(menu-bar-mode               0)
;; (set-cursor-color    "#ffd6f3")
(tool-bar-mode               0)
(vertico-mode                0)
(marginalia-mode             1)
(setq rustic-lsp-client 'eglot)
(pinentry-start)
(load-theme 'doom-laserwave 't)
;; autocomplete
(setq corfu-auto t corfu-quit-no-match 'separator)
(unless (display-graphic-p)
  (corfu-terminal-mode +1))
(global-corfu-mode)

;; ---------------------------------------------------------------------------------------
(add-hook 'eshell-mode-hook         #'eshell-syntax-highlighting-mode)
(add-hook 'eshell-mode-hook         (lambda () (local-set-key (kbd "C-0") #'eshell/clear)))

(add-hook 'prog-mode-hook           #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook           #'smartparens-mode)

(add-hook 'org-mode-hook            #'visual-line-mode)
(add-hook 'erc-mode-hook            #'erc-hl-nicks-enable)
;; global-set-key
(global-set-key (kbd "C-c m")       #'embark-act)
(global-set-key (kbd "C--")         #'text-scale-decrease)
(global-set-key (kbd "C-c r")       #'er/expand-region)
(global-set-key (kbd "C-=")         #'text-scale-increase)
(global-set-key (kbd "C-c e")       #'eshell)
(global-set-key (kbd "C-c l")       #'display-line-numbers-mode)
(global-set-key (kbd "C-x C-k a")   #'kill-other-buffers)
(global-set-key (kbd "C-x w")       #'writeroom-mode)
(global-set-key (kbd "M-j")         #'avy-goto-char)

(define-prefix-command 'custom-command-prefix)
(global-set-key (kbd "C-z") 'custom-command-prefix)
(define-key custom-command-prefix (kbd "s") 'xref-find-definitions)
(define-key custom-command-prefix (kbd "d") 'st-flash)

(put 'overwrite-mode 'disabled t)

;; load meow keybindings from config file
(load-file (concat user-emacs-directory "meow.el"))
(meow-setup)
(meow-global-mode 1)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;; init.el ends here
(put 'erase-buffer 'disabled nil)
