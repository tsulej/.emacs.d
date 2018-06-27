;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/

(require 'cl)

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(require 'package) ;; You might already have this line

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("billpiel" . "http://billpiel.com/emacs-packages/") t)

(package-initialize) ;; You might already have this line

(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
  '(adjust-parens
    aggressive-indent
    auto-complete
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    cider-eval-sexp-fu
    clj-refactor
    paredit
    projectile
    rainbow-mode
    rainbow-delimiters
    multiple-cursors
    markdown-mode
    magit
    yasnippet
    company
    which-key
    smex
    eldoc-eval
    expand-region
    ido-completing-read+
    company-flx
    flx-ido
    clojars
    hideshowvis
    hlinum
    move-text
    project-explorer
    minimap
    use-package
    clojure-snippets
    jdee
    magithub))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'use-package)

(use-package project-explorer
  :bind (("C-x p" . project-explorer-toggle)))

(use-package minimap)

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'post-forward))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq load-prefer-newer t)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package projectile
  :config
  (projectile-mode 1))

(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

(add-to-list 'load-path "~/.emacs.d/customizations")

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :init
  (setq-default save-place t))
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

(setq recentf-save-file (concat user-emacs-directory ".recentf"))

(use-package recentf
  :config
  (recentf-mode 1)
  :init
  (setq recentf-max-menu-items 40))

(load "editing.el")
(load "navigation.el")
(load "setup-clojure.el")

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(defconst backup-dir (concat user-emacs-directory "backups"))
(setq backup-directory-alist `(("." . ,backup-dir)))
(setq auto-save-file-name-transforms `((".*" ,backup-dir t)))
(setq auto-save-list-file-prefix backup-dir)
(setq undo-tree-history-directory-alist `((".*" . ,backup-dir)))
(setq undo-tree-auto-save-history t)
(setq create-lockfiles nil)

(setq auto-window-vscroll nil)

(global-linum-mode 1)
(setq linum-format " %d ")

(require 'hlinum)
(hlinum-activate)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark))
 '(package-selected-packages
   (quote
    (which-key hlinum ido-completing-read+ use-package jdee company-lua lua-mode luarocks shm aggressive-indent project-explorer undo-tree minimap flx-ido move-text expand-region clj-refactor sayid smex rainbow-mode rainbow-delimiters php-mode hideshowvis eldoc-extension eldoc-eval company-flx clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet clojars cider-eval-sexp-fu auto-complete adjust-parens)))
 '(show-paren-mode t)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal)))))
