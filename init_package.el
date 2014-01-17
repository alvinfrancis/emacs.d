(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'cl)
(defun bundle-install-packages (packages)
  (mapc '(lambda (package)
           (unless (package-installed-p package)
             (package-install package)))
        (remove-if 'package-installed-p packages)))

;; Install all of the following packages
(bundle-install-packages (list
                          'ace-jump-mode
                          'alert
                          'auto-complete
                          'centered-cursor-mode
                          'elscreen
                          'eval-sexp-fu
                          'evil
                          'evil-indent-textobject
                          'evil-leader
                          'evil-matchit
                          'evil-nerd-commenter
                          'evil-paredit
                          'evil-tabs
                          'evil-visualstar
                          'goto-chg
                          'helm
                          'highlight
                          'key-chord
                          'monokai-theme
                          'magit
                          'noctilux-theme
                          'paredit
                          'popup
                          'powerline
                          'rainbow-delimiters
                          'rainbow-mode
                          'slime
                          'solarized-theme
                          'surround
                          'undo-tree
                          'vimrc-mode
                          'w3m
                          'zenburn-theme))
