;;; -*- lexical-binding: t; -*-
;;; Code:

(setq gc-cons-threshold most-positive-fixnum)
;;(setq native-comp-deferred-compilation nil)
(setq read-process-output-max (* 1024 1024))

(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "Cascadia Code 16")))
(setq inhibit-splash-screen t)

(setq custom-file
      (concat user-emacs-directory "custom.el"))
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

(package-initialize)

;; setq before package init
(setq monokai-background "#171717")

;;; requires
(dolist (pkg
         '(all-the-icons-ivy-rich
           company
           dashboard
           diredfl
           expand-region
           flycheck
           ivy
           ivy-prescient
           ivy-rich
           ivy-yasnippet
           monokai-theme
           pinentry
           rainbow-delimiters
           smart-mode-line
           smartparens
           swiper
           volatile-highlights
           yasnippet
           no-littering))
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; startup
(dashboard-setup-startup-hook )
(all-the-icons-ivy-rich-mode t)
(beacon-mode                 t)
(diredfl-global-mode          )
(global-company-mode         t)
(global-flycheck-mode        t)
(ivy-mode                    t)
(ivy-prescient-mode          t)
(ivy-rich-mode               t)
(load-theme 'monokai         t)
(menu-bar-mode               0)
(tool-bar-mode               0)
(volatile-highlights-mode    t)
(pinentry-start               )

(setq ivy-use-virtual-buffers   t)
(setq ivy-height                15)
(setq ivy-use-virtual-buffers   t)
(setq sml/theme                 'atom-one-dark)
(sml/setup                      t)

;;(load-file (concat user-emacs-directory "packages/hare-mode/hare-mode.el"))
(load-file (concat user-emacs-directory "packages/ligature/ligature.el"))

;;; hooks
;; function definitions
(defun hl-custom ()
  "Force custom theme highlight in c-mode-hook."
  (font-lock-add-keywords nil '(("[*0-9]" 0 font-lock-keyword-face t))))
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(defun freebsd-style ()
  "Enable FreeBSD style for C buffers."
  (progn (load-file "/usr/src/tools/tools/editing/freebsd.el") (bsd)))
(ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                     ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                     "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                     "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                     "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                     "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                     "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                     "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                     ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                     "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                     "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                     "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                     "\\\\" "://"))
;; hooks
(add-hook 'prog-mode-hook           #'ligature-mode)
(add-hook 'org-mode-hook            #'org-html-themify-mode)
(add-hook 'prog-mode-hook           #'yas-minor-mode)
(add-hook 'prog-mode-hook           #'smartparens-mode)
(add-hook 'c-mode-hook              #'hl-custom)
(add-hook 'c-mode-hook              #'freebsd-style)
(add-hook 'eshell-mode-hook         #'eshell-syntax-highlighting-mode)
(add-hook 'prog-mode-hook           #'rainbow-delimiters-mode)
;; global-set-key
(global-set-key (kbd "C-=")         #'text-scale-increase)
(global-set-key (kbd "C--")         #'text-scale-decrease)
(global-set-key (kbd "C-s")         #'swiper-isearch)
(global-set-key (kbd "M-x")         #'counsel-M-x)
(global-set-key (kbd "C-c y")       #'ivy-yasnippet)
(global-set-key (kbd "C-:")         #'er/expand-region)
(global-set-key (kbd "C-c l")       #'display-line-numbers-mode)
(global-set-key (kbd "C-x C-f")     #'counsel-find-file)
(global-set-key (kbd "C-x C-n")     #'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x w")       #'writeroom-mode)
(global-set-key (kbd "C-x C-k a")   #'kill-other-buffers)
(global-set-key (kbd "C-c e")       #'eshell)

;;; init.el ends here
(put 'overwrite-mode 'disabled t)
