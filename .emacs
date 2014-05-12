; initialize stuff

(load "~/.emacs.d/init_package.el")
(load "~/.emacs.d/init.el")
(load "~/.emacs.d/init_evil.el")
(load "~/.emacs.d/init_helm.el")
(load "~/.emacs.d/init_org.el")

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

(setq view-diary-entries-initially t
      mark-diary-entries-in-calendar t
      number-of-diary-entries 7)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(load-theme 'bubbleberry t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "#505D6B" :foreground "#B0E6FF"))))
 '(helm-source-header ((t (:background "#3E6B6B" :foreground "#7FFFFD" :underline nil :weight bold :height 1))))
 '(hl-sexp-face ((t (:background "#292929"))) t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("0c311fb22e6197daba9123f43da98f273d2bfaeeaeb653007ad1ee77f0003037" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "aa392ee7deac22c0c0b71b396d4969ba0849b538bb1790ef31b115b4e620c0b5" "9c26d896b2668f212f39f5b0206c5e3f0ac301611ced8a6f74afe4ee9c7e6311" "36d0f600074e9299fb7b6a316161d99faa16a6551ddeda50980ae293e653e7b4" default)))
 '(fringe-mode nil nil (fringe))
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945"))
