(setq default-directory "~/")
(global-hl-line-mode t)
(global-linum-mode t)
(recentf-mode t)
(set-default 'truncate-lines t)
(setq auto-save-default nil)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq scroll-margin 100)
(setq-default indent-tabs-mode nil)
(show-paren-mode t)
(setq initial-scratch-message "")
(if window-system
  (progn
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;; packages
(powerline-center-evil-theme)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 
(global-rainbow-delimiters-mode)
(setq alert-default-style 'notifier)
(global-surround-mode t)

(setq inferior-lisp-program "/opt/local/bin/sbcl")
(slime-setup '(slime-fancy))

;; eval-sexp-fu requires cl and highlight to work
;; require is enough to turn on eval-sexp-fu
(and 
 (require 'cl)
 (require 'highlight)
 (require 'eval-sexp-fu))

;; Looks like centered-cursor-mode isn't autoloaded like the others
;; (and
;;  (require 'centered-cursor-mode)
;;  (global-centered-cursor-mode t))
