(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)
(defun bundle-install-packages (packages)
  "Install each package in packages if not yet installed."
  (mapc '(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        (remove-if 'package-installed-p packages)))

;; Install all of the following packages
(bundle-install-packages '(ace-jump-mode
                           adaptive-wrap
                           alert
                           auto-complete
                           centered-cursor-mode
                           dtrt-indent
                           elscreen
                           ensime
                           eval-sexp-fu
                           evil
                           evil-indent-textobject
                           evil-leader
                           evil-matchit
                           evil-nerd-commenter
                           evil-paredit
                           evil-tabs
                           evil-visualstar
                           goto-chg
                           helm
                           highlight
                           hl-sexp
                           key-chord
                           monokai-theme
                           git-gutter+
                           magit
                           noctilux-theme
                           paredit
                           popup
                           powerline
                           rainbow-delimiters
                           rainbow-mode
                           slime
                           solarized-theme
                           surround
                           trident-mode
                           undo-tree
                           vimrc-mode
                           web-mode
                           w3m
                           zenburn-theme))
