(setq default-directory "~/")
(global-hl-line-mode t)
(global-nlinum-mode t)
(recentf-mode t)
(set-default 'truncate-lines t)
(setq auto-save-default nil)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq scroll-margin 100)
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(show-paren-mode t)
(setq initial-scratch-message "")
(setq nlinum-format " %d ")
(set-face-attribute 'default nil :font "Monaco-11")
(setq tab-width 4)
(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(setq fill-column 80)
(menu-bar-mode -1)
(electric-indent-mode 1)
(if window-system
  (progn
    (scroll-bar-mode -1)
    (tool-bar-mode -1)))

;; mac-stuff
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; package-dependent
(powerline-center-evil-theme)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 
(global-rainbow-delimiters-mode)
(setq alert-default-style 'notifier)
(global-surround-mode t)
;; (global-hl-sexp-mode t)

(setq inferior-lisp-program "/opt/local/bin/sbcl")

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

;;;; Pending organization

;; JS2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)) 

;; slime
(slime-setup '(slime-fancy slime-asdf))
(add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Trident-mode
(add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
(add-hook 'lisp-mode-hook
          #'(lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.paren\\>" buffer-file-name))
                (unless (slime-connected-p)
                  (save-excursion (slime)))
                (trident-mode +1))))
(add-hook 'trident-mode-hook (lambda () (trident-add-keys-with-prefix "C-c C-e")))

;; Transparency
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Powerline
(setf powerline-default-separator nil)

;; Lisp indenting
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

;; git
(when window-system
  (require 'git-gutter-fringe+))

;; Ace Mode
(global-set-key (kbd "s-f") 'evil-ace-jump-char-mode)

;; smart-tabs-mode
(smart-tabs-add-language-support csharp csharp-mode-hook
  ((c-indent-line . c-basic-offset)
   (c-indent-region . c-basic-offset)))
(smart-tabs-insinuate 'c 'c++ 'csharp 'java 'javascript 'cperl 'python 'ruby 'nxml)

;; csharp-mode hook
(add-hook 'csharp-mode-hook
          (lambda ()
            (electric-pair-mode)
            (setq electric-pair-pairs '((?\" . ?\")
                                        (?\{ . ?\})
                                        (?\< . ?\>)))
            ;; No easy way to disable using csharp-insert-open-brace.
            (local-set-key (kbd "{") 'self-insert-command)))
