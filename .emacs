; initialize stuff

(load "~/.emacs.d/init_package.el")
(load "~/.emacs.d/init.el")
(load "~/.emacs.d/init_evil.el")
(load "~/.emacs.d/init_helm.el")
(load "~/.emacs.d/init_org.el")
(load "~/.emacs.d/init_ensime.el")
(load "~/.emacs.d/init-multiple-cursors.el")

;; vimrc major mode
(add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))

;; Copied from EmacsWiki -- enable ParEdit for everywhere
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

(defadvice load-theme 
  (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))
(defadvice load-theme (after hide-vertical-border activate)
  (set-face-attribute 'vertical-border nil :foreground
                      (face-attribute 'default :background)))
(load-theme 'fogus t)

(setq
 custom-file (concat (file-name-as-directory user-emacs-directory) "custom.el"))
(load custom-file)
