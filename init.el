;;; -*- lexical-binding: t; -*-
;;; Code:

;; Prevent initial white flash on startup
(set-background-color "#171717")

(setq gc-cons-threshold most-positive-fixnum)
;;(setq native-comp-deferred-compilation nil)
(setq read-process-output-max (* 1024 1024))

(setq initial-frame-alist
      '((height . 30)
        (width . 80)))
(setq default-frame-alist
      '(;(fullscreen . maximized)
        (font . "Cascadia Code 18")))
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
;;(package-initialize)

;; setq before package init
(setq monokai-background "#171717")

;;; requires
(dolist (pkg
         '(
           all-the-icons-ivy-rich
           company
           dashboard
           diredfl
           expand-region
           flycheck
           ivy
           ivy-posframe
           ivy-prescient
           ivy-rich
           ivy-yasnippet
           keycast
           monokai-theme
           no-littering
           pinentry
           rainbow-delimiters
           smart-mode-line
           smartparens
           swiper
           volatile-highlights
           yasnippet
           ))
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

;;; startup
(all-the-icons-ivy-rich-mode t)
(beacon-mode                 t)
(dashboard-setup-startup-hook )
(diredfl-global-mode          )
(global-company-mode         t)
(global-flycheck-mode        t)
(ivy-mode                    t)
(ivy-posframe-mode)
(ivy-prescient-mode          t)
(ivy-rich-mode               t)
(keycast-mode                t)
(load-theme 'monokai         t)
(menu-bar-mode               0)
(scroll-bar-mode             0)
(pinentry-start               )
(pixel-scroll-precision-mode t)
(tool-bar-mode               0)
(volatile-highlights-mode    t)

;; smooth-scroll
(setq pixel-scroll-precision-large-scroll-height 40.0)
(setq pixel-scroll-precision-interpolation-factor 30)

;;; package setq's
(setq ivy-height                15)
(setq ivy-use-virtual-buffers   t)
(setq ivy-use-virtual-buffers   t)
(setq sml/theme                 'atom-one-dark)
(sml/setup                      t)
(setq dashboard-startup-banner
      (concat user-emacs-directory "themes/emacs-p.svg"))
(load-file (concat user-emacs-directory
                   "packages/ligature/ligature.el"))
(load-file (concat user-emacs-directory
                   "packages/maple-echoarea/maple-echoarea.el"))
(maple-echoarea-enable)
(ligature-set-ligatures 'prog-mode
                        '("|||>" "<|||" "<==>" "<!--"
                          "####" "~~>" "***" "||=" "||>"
                          ":::" "::=" "=:=" "===" "==>"
                          "=!=" "=>>" "=<<" "=/=" "!=="
                          "!!." ">=>" ">>=" ">>>" ">>-"
                          ">->" "->>" "-->" "---" "-<<"
                          "<~~" "<~>" "<*>" "<||" "<|>"
                          "<$>" "<==" "<=>" "<=<" "<->"
                          "<--" "<-<" "<<=" "<<-" "<<<"
                          "<+>" "</>" "###" "#_(" "..<"
                          "..." "+++" "/==" "///" "_|_"
                          "www" "&&" "^=" "~~" "~@" "~="
                          "~>" "~-" "**" "*>" "*/" "||"
                          "|}" "|]" "|=" "|>" "|-" "{|"
                          "[|" "]#" "::" ":=" ":>" ":<"
                          "$>" "==" "=>" "!=" "!!" ">:"
                          ">=" ">>" ">-" "-~" "-|" "->"
                          "--" "-<" "<~" "<*" "<|" "<:"
                          "<$" "<=" "<>" "<-" "<<" "<+"
                          "</" "#{" "#[" "#:" "#=" "#!"
                          "##" "#(" "#?" "#_" "%%" ".="
                          ".-" ".." ".?" "+>" "++" "?:"
                          "?=" "?." "??" ";;" "/*" "/="
                          "/>" "//" "__" "~~" "(*" "*)"
                          "\\\\" "://"))

;;; function definitions
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

;;; hooks
(add-hook 'c-mode-hook              #'freebsd-style)
(add-hook 'c-mode-hook              #'hl-custom)
(add-hook 'eshell-mode-hook         #'eshell-syntax-highlighting-mode)
(add-hook 'org-mode-hook            #'org-html-themify-mode)
(add-hook 'prog-mode-hook           #'ligature-mode)
(add-hook 'prog-mode-hook           #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook           #'smartparens-mode)
(add-hook 'prog-mode-hook           #'yas-minor-mode)
;; global-set-key
(global-set-key (kbd "C--")         #'text-scale-decrease)
(global-set-key (kbd "C-:")         #'er/expand-region)
(global-set-key (kbd "C-=")         #'text-scale-increase)
(global-set-key (kbd "C-c e")       #'eshell)
(global-set-key (kbd "C-c l")       #'display-line-numbers-mode)
(global-set-key (kbd "C-c y")       #'ivy-yasnippet)
(global-set-key (kbd "C-s")         #'swiper-isearch)
(global-set-key (kbd "C-x C-f")     #'counsel-find-file)
(global-set-key (kbd "C-x C-k a")   #'kill-other-buffers)
(global-set-key (kbd "C-x C-n")     #'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x w")       #'writeroom-mode)
(global-set-key (kbd "M-x")         #'counsel-M-x)

(put 'overwrite-mode 'disabled t)
;;; init.el ends here
