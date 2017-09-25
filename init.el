;; https://github.com/flyingmachine/emacs-for-clojure/blob/master/

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
    eldoc-extension
    eldoc-eval
    expand-region
    ido-completing-read+
    company-flx
    flx-ido
    clojars
    hideshowvis
    move-text
    project-explorer
    minimap
    clojure-snippets
    haskell-snippets
    magithub))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'project-explorer)
(global-set-key (kbd "C-x p") 'project-explorer-toggle)

(require 'minimap)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

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

(require 'yasnippet)
(yas-global-mode 1)

(require 'projectile)
(projectile-mode 1)

(add-to-list 'load-path "~/.emacs.d/customizations")

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

(load "editing.el")
(load "navigation.el")
(load "setup-clojure.el")

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

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
    (ido-ubiquitous company-lua lua-mode luarocks ido-completing-read+ shm haskell-snippets aggressive-indent magithub project-explorer undo-tree minimap flx-ido move-text expand-region markdown-mode clj-refactor sayid which-key smex rainbow-mode rainbow-delimiters projectile php-mode magit hideshowvis eldoc-extension eldoc-eval company-flx clojure-snippets clojure-mode-extra-font-locking clojure-cheatsheet clojars cider-eval-sexp-fu auto-complete adjust-parens)))
 '(show-paren-mode t)
 '(size-indication-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 90 :width normal)))))
