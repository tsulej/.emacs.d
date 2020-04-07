;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; yay rainbows!
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;(setq electric-indent-mode nil)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(with-eval-after-load 'company
  (company-flx-mode +1))

(use-package adjust-parens)

(use-package expand-region
  :bind ("C-@" . er/expand-region))

(use-package  multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-<tab>") 'indent-relative)

;; keyboard macros

(fset 'notespace-wrap-plot
      [return up ?\( ?n ?o ?t ?e ?- ?m ?d ?  ?\" ?# ?# ?# ?\S-  ?F ?i ?g ?u ?r ?e ?\C-e down ?\C-a ?\( ?n ?o ?t ?e ?- ?v ?o ?i ?d C-right ?  ?\( ?p ?l ?o ?t ?- ?> ?f ?i ?l ?e C-right ?  ?\( ?s ?t ?r ?  ?t ?a ?r ?g ?e ?t ?- ?p ?a ?t ?h ?  ?\" ?. ?p ?n ?g down up ?\C-a right ?\) return ?\( ?n ?o ?t ?e ?- ?h ?i ?c ?c ?u ?p ?  ?\[ ?: ?i ?m ?a ?e ?  backspace backspace ?g ?e ?  ?\{ ?: ?s ?r ?c ?  ?\" ?. ?p ?n ?g left left left left])
