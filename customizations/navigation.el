(use-package flx-ido
  :config
  (ido-mode t)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces t)
  (setq ido-use-filename-at-point nil)

  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  (setq ido-auto-merge-work-directories-length -1)

  ;; Includes buffer names of recently open files, even if they're not
  ;; open now
  (setq ido-use-virtual-buffers t))

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode 1))

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :init
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex))

(defun prev-window ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-M-<next>")  'other-window)
(global-set-key (kbd "C-M-<prior>")  'prev-window)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package which-key
  :config
  (which-key-mode))

(use-package move-text
  :config
  (move-text-default-bindings))
