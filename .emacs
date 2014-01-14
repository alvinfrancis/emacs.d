; initialize stuff

(load "~/.emacs.d/init_package.el")
(load "~/.emacs.d/init.el")
(load "~/.emacs.d/init_evil.el")
(load "~/.emacs.d/init_helm.el")

;; vimrc major mode
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;; Copied from EmacsWiki -- enable ParEdit for everywhere
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(require 'auto-complete)
(add-hook 'emacs-lisp-mode-hook 'auto-complete-mode)
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" "9370aeac615012366188359cb05011aea721c73e1cb194798bc18576025cabeb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))

(load-theme 'solarized-dark t)
