;;;; package
(require 'package)

(setq
 package-user-dir (expand-file-name (file-name-as-directory "elpa") user-emacs-directory)
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; req-package should ideally be loaded by something other than package
;; (package-refresh-contents)
(mapc (lambda (p)
        (unless (package-installed-p p)
          (package-install p)))
      '(use-package req-package))

(require 'req-package)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;; Basic
;; clean UI
(setq
 scroll-margin 100          ; TODO: look into a better centering mechanism
 scroll-conservatively 100
 initial-scratch-message "" ; maybe make this into cowsay
 inhibit-splash-screen t
 )

(menu-bar-mode -1)
(if window-system
    (progn
      ;; Remove GUI cruft even in GUI Emacs
      (scroll-bar-mode -1)
      (tool-bar-mode -1)

      ;; Support ligatures
      (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                     (35 . ".\\(?:[(?[_{]\\)")
                     (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                     (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
                     (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                     (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                     (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
                     (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                     (58 . ".\\(?:[:=]\\)")
                     (59 . ".\\(?:;\\)")
                     (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                     (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                     (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                     (63 . ".\\(?:[:=?]\\)")
                     (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                     (94 . ".\\(?:=\\)")
                     (123 . ".\\(?:-\\)")
                     (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                     (126 . ".\\(?:[=@~-]\\)"))))
        (dolist (char-regexp alist)
          (set-char-table-range composition-function-table (car char-regexp)
                                `([,(cdr char-regexp) 0 font-shape-gstring]))))
      ))

;; mac-stuff
(setq
 mac-option-modifier 'meta              ; prefer to use option as Meta
 mac-command-modifier 'super            ; command for Super
 delete-by-moving-to-trash t
 trash-directory "~/.Trash"             ; play well with Trash
 ns-use-native-fullscreen nil
 )

(defun fullscreen-toggle ()
  (interactive)
  (let ((frame (window-frame (posn-window (event-start nil)))))
    (if (not (eq (frame-parameter frame 'fullscreen) 'fullboth))
        (set-frame-parameter frame 'fullscreen 'fullboth)
      (set-frame-parameter frame 'fullscreen nil))))

(bind-key "<f9>" 'fullscreen-toggle)

;; others
(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 recentf-mode t
 recentf-max-saved-items 500
 visible-bell t                         ; prefer visual over beeping
 fill-column 80
 )

;;;; Defaults
(setq-default
 indent-tabs-mode nil         ; whitespace > tabs
 tab-width 4                  ;
 truncate-lines t             ; do not wrap by default
 word-wrap t                  ; but turn on word wrap if line wrapping
 )

;;;; Org-mode
(req-package org
  :require (htmlize evil)
  :commands org-mode
  :init (progn
          (require 'org-crypt)
          (setq org-fontify-quote-and-verse-blocks t
                org-src-fontify-natively t
                org-tags-exclude-from-inheritance (quote ("crypt"))
                org-crypt-key "alvin.francis.dumalus@gmail.com"
                org-hide-emphasis-markers t
                org-adapt-indentation nil)
          (set-face-attribute 'org-block-background nil
                              :background "#070707")
          (set-face-attribute 'org-block-begin-line nil
                              :background "#002D43")
          (set-face-attribute 'org-block-end-line nil
                              :background "#002D43")
          (evil-define-key 'normal org-mode-map
            (kbd "> >") 'org-indent-item
            (kbd "> t") 'org-indent-item-tree
            (kbd "< <") 'org-outdent-item
            (kbd "< t") 'org-outdent-item-tree))
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (sql . t)))
            (org-crypt-use-before-save-magic)))

(req-package org-journal
  :require org
  :commands org-journal-mode
  :init (progn
          (setq org-journal-file-format "%Y-%m-%d.org")
          (add-hook 'org-journal-mode-hook 'auto-fill-mode)))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; (req-package sublime-themes)
;; (req-package monokai-theme)
;; (req-package bubbleberry-theme)

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defadvice load-theme (after hide-vertical-border activate)
  (set-face-attribute 'vertical-border nil :foreground
                      (face-attribute 'default :background)))

(req-package linum
  :init (progn
          (setq linum-format " %d ")
          (add-hook 'prog-mode-hook 'linum-mode)))

(req-package paren
  :defer 60
  :config (show-paren-mode t))

(req-package git-gutter+
  :config (global-git-gutter+-mode t))

(req-package evil-surround
  :require evil
  :config (progn
            (evil-define-key 'visual evil-surround-mode-map
              "s" nil
              "S" 'evil-surround-region)
            (global-evil-surround-mode)))

(req-package elscreen
  :require evil
  :init (progn
          (setq elscreen-tab-display-control nil
                elscreen-display-screen-number nil
                elscreen-tab-display-kill-screen nil)

          (defun evil-elscreen-quit ()
            (interactive)
            (if (and (not (elscreen-one-screen-p))
                     (one-window-p))
                (elscreen-kill)
              (evil-quit)))

          (evil-ex-define-cmd "quit" #'evil-elscreen-quit)

          (bind-keys
           :map evil-normal-state-map
           (", n n" . elscreen-create)
           (", t c" . elscreen-kill)
           (", t T" . elscreen-toggle-display-tab)
           ("H" . elscreen-previous)
           ("L" . elscreen-next)))
  :config (progn
            (elscreen-toggle-display-screen-number)
            (elscreen-start)))

(req-package rainbow-delimiters
  :init (progn
          (setq rainbow-delimiters-max-face-count 1)
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  :commands rainbow-delimiters-mode
  :config (progn
            (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                                :foreground 'unspecified
                                :inherit 'error)))

(req-package powerline
  :require evil
  :init (setf powerline-default-separator nil)
  :config (progn
            (defadvice load-theme (after reset-powerline activate)
              (powerline-reset))
            (powerline-center-evil-theme)))

(req-package paredit
  :require (cl evil)
  :init (evil-define-key 'normal paredit-mode-map
          (kbd "< (") 'paredit-backward-slurp-sexp
          (kbd "> )") 'paredit-forward-slurp-sexp
          (kbd "> (") 'paredit-backward-barf-sexp
          (kbd "< )") 'paredit-forward-barf-sexp
          (kbd "W") 'paredit-forward
          (kbd "B") 'paredit-backward)
  :config (progn
            (cl-labels ((enable-paredit (hook) (add-hook hook #'enable-paredit-mode)))
              (mapc #'enable-paredit '(emacs-lisp-mode-hook
                                       cider-repl-mode-hook
                                       eval-expression-minibuffer-setup-hook
                                       clojure-mode-hook
                                       ielm-mode-hook
                                       lisp-mode-hook
                                       lisp-interaction-mode-hook
                                       scheme-mode-hook)))))

(req-package slime
  :require (evil paredit)
  :init (evil-define-key 'normal slime-mode-map
          (kbd ", x e") 'slime-eval-last-expression
          (kbd ", x x") 'slime-eval-defun
          (kbd ", x p") 'slime-pprint-eval-last-expression)
  :command slime
  :config (progn
            (slime-setup '(slime-fancy slime-asdf))
            (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)))

(req-package ido
  :init (setq ido-enable-flex-matching t
              ido-everywhere t)
  :config (ido-mode 1))

(req-package git-gutter-fringe+
  :if window-system)

;;;; Languages
(req-package ensime
  :require evil
  :commands ensime
  :init (evil-define-key 'normal ensime-mode-map
          (kbd "C-]") 'ensime-edit-definition)
  :config (progn
            (defun setup-ensime ()
              (defvar ensime-slick-prefix "^scala\\.slick\\.")

              (defvar ensime-slickdoc-url-base
                "http://slick.typesafe.com/doc/2.0.0-M3/api/index.html#"
                "URL base for constructing slick links.")

              (defun ensime-make-slick-doc-url-helper
                  (url-base type &optional member)
                "Given a scala type, and optionally a type member,
   construct the corresponding slick url.  Currently does not
   narrow down to member"
                (concat url-base (ensime-type-full-name type)))

              (defun ensime-make-slick-doc-url (type &optional member)
                (ensime-make-slick-doc-url-helper
                 ensime-slickdoc-url-base type member))
              )
            (add-hook 'ensime-connected-hook 'setup-ensime)))

(req-package scala-mode
  :commands scala-mode
  :config (progn
            (add-hook 'scala-mode-hook
                      (lambda ()
                        (setq imenu-generic-expression
                              '(("var" "\\(var +\\)\\([^(): ]+\\)" 2)
                                ("val" "\\(val +\\)\\([^(): ]+\\)" 2)
                                ("override def" "^[ \\t]*\\(override\\) +\\(def +\\)\\([^(): ]+\\)" 3)
                                ("implicit def" "^[ \\t]*\\(implicit\\) +\\(def +\\)\\([^(): ]+\\)" 3)
                                ("def" "^[ \\t]*\\(def +\\)\\([^(): ]+\\)" 2)
                                ("trait" "\\(trait +\\)\\([^(): ]+\\)" 2)
                                ("class" "^[ \\t]*\\(class +\\)\\([^(): ]+\\)" 2)
                                ("case class" "^[ \\t]*\\(case class +\\)\\([^(): ]+\\)" 2)
                                ("object" "\\(object +\\)\\([^(): ]+\\)" 2))
                              )))))

(req-package trident-mode
  :require (slime skewer-mode)
  :mode ("\\.paren\\'" . lisp-mode)
  :commands trident-mode
  :config (progn
            (add-hook 'lisp-mode-hook
                      (lambda ()
                        (when (and buffer-file-name
                                   (string-match-p "\\.paren\\>" buffer-file-name))
                          (unless (slime-connected-p)
                            (save-excursion (slime)))
                          (trident-mode +1))))
            (add-hook 'trident-mode-hook
                      (lambda () (trident-add-keys-with-prefix "C-c C-e")))))

(req-package csharp-mode
  :require electric
  :commands csharp-mode
  :config (progn
            (electric-indent-mode t)
            (add-hook 'csharp-mode-hook
                      (lambda ()
                        (electric-pair-mode)
                        (setq electric-pair-pairs '((?\" . ?\")
                                                    (?\{ . ?\})
                                                    (?\< . ?\>)))))))

(req-package vimrc-mode
  :commands vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

(req-package js2-mode)

(req-package jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode))

(req-package web-mode
  :require evil
  :init (progn
          (setq web-mode-code-indent-offset 2)
          (evil-define-key 'operator web-mode-map
            (kbd "g c") 'web-mode-comment-or-uncomment)))

(req-package cider
  :require evil
  :commands cider-mode
  :init (progn
          (evil-define-key 'normal cider-mode-map
            (kbd ", x p") 'cider-eval-print-last-sexp
            (kbd ", x e") 'cider-eval-last-sexp
            (kbd ", x x") 'cider-eval-defun-at-point
            (kbd "C-]") 'cider-jump-to-var)
          (evil-define-key 'visual cider-mode-map
            (kbd ", x r") 'cider-eval-region)
          (evil-define-key 'normal cider-popup-buffer-mode-map
            "q" 'cider-popup-buffer-quit-function)
          (evil-define-key 'normal cider-stacktrace-mode-map
            "q" 'cider-popup-buffer-quit-function)
          (evil-define-key 'normal cider-docview-mode-map
            "q" 'cider-popup-buffer-quit-function))
  :config (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(req-package ac-cider
  :require (cider auto-complete)
  :bind (:map cider-mode-map
              ("C-c C-d" . ac-cider-popup-doc)))

(req-package clojure-mode
  :require (evil)
  :commands clojure-mode
  :config (progn
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2)
              (match 1))
            (put-clojure-indent 'reify '(:defn (1)))
            (put-clojure-indent 'defprotocol '(:defn (1)))
            (put-clojure-indent 'this-as '(:defn (1)))
            (evil-define-key 'normal clojure-mode-map
              (kbd "C-c C-e") 'lisp-eval-last-sexp
              (kbd "C-c C-c") 'lisp-eval-defun)
            (evil-define-key 'visual clojure-mode-map
              (kbd "C-c C-r") 'lisp-eval-region)))

(req-package clojure-cheatsheet
  :commands clojure-cheatsheet)

;; SQL
(req-package sql
  :init (progn
          (setq sql-product 'postgres
                sql-server "localhost")
          (add-hook 'sql-interactive-mode-hook
                    (lambda ()
                      (unbind-key ";" sql-interactive-mode-map))))
  :config (defun sql-send-paragraph ()
            "Send the current paragraph to the SQL process."
            (interactive)
            (let ((start (save-excursion
                           (backward-paragraph)
                           (point)))
                  (end (save-excursion
                         (forward-paragraph)
                         (point))))
              (sql-send-string
               (mapconcat #'identity
                          (split-string (buffer-substring start end) "\n")
                          " ")))))

;; Lisp
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
(setq inferior-lisp-program "/opt/local/bin/sbcl")

;; Emacs Lisp
(cl-labels ((help-prefix (key command)
                         (define-key 'help-command key command)))
  (help-prefix (kbd "C-f") 'find-function)
  (help-prefix (kbd "C-k") 'find-function-on-key)
  (help-prefix (kbd "C-l") 'find-library)
  (help-prefix (kbd "C-v") 'find-variable))

;; HTML
(add-hook 'html-mode-hook
          (lambda ()
            (progn
              (setq-local tab-width 2))))

;; YAML
(req-package yaml-mode
  :commands yaml-mode)

(req-package eldoc
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;;;; Evil
(req-package evil
  :require (key-chord ace-jump-mode flymake)
  :init (setq evil-move-cursor-back nil
              evil-search-module 'evil-search
              evil-want-fine-undo nil)
  :config (progn
            (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
            (key-chord-define evil-replace-state-map "jj" 'evil-normal-state)

            (defun evil-yank-to-end-of-line ()
              "Yank to end of line"
              (interactive)
              (evil-yank (point) (point-at-eol)))
            (defun evil-search-word-forward-stay ()
              (interactive)
              (evil-search-word-backward)
              (evil-search-word-forward))
            (defun evil-search-word-backward-stay ()
              (interactive)
              (evil-search-word-forward)
              (evil-search-word-backward))

            (evil-define-motion evil-ace-jump-char-mode (count)
              :type exclusive
              (ace-jump-mode 5)
              (recursive-edit))
            (evil-define-motion evil-jump-up (count)
              (evil-previous-line (* (or count 1) 5)))
            (evil-define-motion evil-jump-down (count)
              (evil-next-line (* (or count 1) 5)))
            (evil-define-operator evil-toggle-comment (beg end)
              "Comment operator that can work with evil-motions."
              (comment-or-uncomment-region beg end))

            ;; Matching Vim's set hls behaviour
            (evil-ex-define-cmd "hls"
                                #'(lambda ()
                                    (interactive)
                                    (setf evil-ex-search-highlight-all t)
                                    (evil-ex-search-activate-highlight evil-ex-search-pattern)))
            (evil-ex-define-cmd "nohls"
                                #'(lambda ()
                                    (interactive)
                                    (setf evil-ex-search-highlight-all nil)
                                    (evil-ex-nohighlight)))

            (unbind-key (kbd "K") evil-motion-state-map)
            (unbind-key (kbd "C-n") evil-insert-state-map)
            (unbind-key (kbd "C-p") evil-insert-state-map)
            (bind-keys
             :map evil-normal-state-map
             (";" . evil-ex)
             ("<left>" . evil-prev-buffer)
             ("<right>" . evil-next-buffer)
             ("Y" . evil-yank-to-end-of-line)
             ("g c" . evil-toggle-comment)
             (", x p" . eval-print-last-sexp)
             (", x e" . eval-last-sexp)
             (", x x" . eval-defun)
             (", x r" . eval-region))
            (bind-keys
             :map evil-visual-state-map
             (";" . evil-ex))
            (bind-keys
             :map evil-motion-state-map
             ("s-f" . evil-ace-jump-char-mode)
             ("C-u" . evil-scroll-up)
             ("-" . evil-jump-up)
             ("SPC" . evil-jump-down)
             (":" . evil-repeat-find-char)
             ("*" . evil-search-word-forward-stay)
             ("#" . evil-search-word-backward-stay))

            (add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)
            (evil-mode t)))

(req-package evil-visualstar
  ;; Problems with using n and N to continue the search
  :require evil
  :config (global-evil-visualstar-mode))

(req-package evil-leader
  :require evil)

(req-package evil-indent-textobject
  :require evil)

(req-package evil-easymotion
  :require evil
  :config (evilem-default-keybindings "M-SPC"))

;;;; Utilities
(req-package magit
  :require (evil hl-line)
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (evil-ex-define-cmd "Gstatus" #'magit-status)
          (evil-ex-define-cmd "Gs" "Gstatus"))
  :config (progn
            (add-hook 'magit-mode-hook
                      (lambda ()
                        (hl-line-mode)))
            (bind-key "<f10>" 'magit-status evil-normal-state-map)))

(req-package hippie-exp
  :require evil
  :bind (:map evil-insert-state-map
              ("M-TAB" . hippie-expand)))

(req-package key-chord
  :init (setq key-chord-one-key-delay 1.0
              key-chord-two-keys-delay 1.0)
  :config (progn
            (key-chord-mode t)))

(req-package adaptive-wrap
  :init (setq-default adaptive-wrap-extra-indent 2)
  :config (progn
            (when (fboundp 'adaptive-wrap-prefix-mode)
              (defun visual-line-adaptive-wrap-prefix-mode ()
                (adaptive-wrap-prefix-mode visual-line-mode)))
            ;; (add-hook
            ;;  'visual-line-mode-hook
            ;; 'visual-line-adaptive-wrap-prefix-mode)
 ))

(req-package projectile)

(req-package helm
  :require (evil helm-ls-git)
  :config (progn
            (set-face-attribute 'helm-selection nil
                                :background "#505D6B"
                                :foreground "#B0E6FF"
                                )
            (set-face-attribute 'helm-source-header nil
                                :background "#3E6B6B"
                                :foreground "#7FFFFD"
                                :underline nil
                                :weight 'normal
                                :height 1
                                )
            (defun helm-files-mru ()
              (interactive)
              (helm :sources '(helm-source-recentf
                               helm-source-findutils
                               helm-source-files-in-current-dir)
                    :buffer "*helm-files-mru-buffers*"))
            (bind-keys
             ("M-x" . helm-M-x)
             ("C-x C-f" . helm-find-files))
            (bind-keys
             :map evil-normal-state-map
             (", ;" . helm-M-x)
             ("C-p" . helm-files-mru)
             ("\\ f" . helm-browse-project)
             ("\\ b" . helm-buffers-list)
             ("\\ y" . helm-show-kill-ring)
             ("\\ m" . helm-imenu)
             ("\\ \\" . helm-resume))
            (bind-key ", ;" 'helm-M-x evil-visual-state-map)
            (helm-mode t)))

(req-package helm-swoop
  :require (evil helm)
  :config (bind-key "\\ l" 'helm-swoop evil-normal-state-map))

(req-package helm-descbinds
  :require helm)

(req-package auto-complete
  :bind (:map ac-complete-mode-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous))
  :require evil
  :config (ac-config-default))

(req-package helm-ag)

(req-package helm-projectile
  :require (evil helm projectile helm-ag)
  :config (bind-keys
           :map evil-normal-state-map
           ("\\ p p" . helm-projectile)
           ("\\ p a" . helm-projectile-ag)))

(req-package eval-sexp-fu)

(req-package restclient)

(cl-labels
    ((package-menu-evil ()
       (progn
         (evil-normal-state)
         (bind-keys
          :map evil-normal-state-local-map
          ("i" . package-menu-mark-install)
          ("d" . package-menu-mark-delete)
          ("U" . package-menu-mark-upgrades)
          ("u" . package-menu-mark-unmark)
          ("RET" . package-menu-describe-package)
          ("q" . quit-window)
          ("x" . package-menu-execute)))))
  (add-hook 'package-menu-mode-hook #'package-menu-evil))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(req-package which-func
  :bind ("C-x 4 n" . clone-buffer-and-narrow-to-function)
  :commands which-function
  :init (defun clone-buffer-and-narrow-to-function ()
          (interactive)
          (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
          (mark-defun) ; works not only in emacs-lisp, but C++, Python, ...
          (narrow-to-region (mark) (point))
          (pop-mark)
          (other-window 1)))

(req-package multiple-cursors
  :require evil
  :disabled t
  :init (progn
          (global-set-key (kbd "s-<up>") 'mc/mark-previous-like-this)
          (global-set-key (kbd "s-<down>") 'mc/mark-next-like-this)
          (bind-keys :map evil-visual-state-map
                     ("C-n" . mc/mark-next-like-this)
                     ("C-p" . mc/mark-previous-like-this))))

(req-package linum-relative
  :init (setq linum-relative-format " %3s "))

(req-package highlight
  :require evil
  :bind (:map evil-normal-state-map
              (", h h" . hlt-highlight-symbol)
              (", h x" . hlt-unhighlight-symbol)))

(req-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(req-package iedit
  :bind ("C-;" . iedit-mode))

(req-package ssh
  :config (add-hook 'ssh-mode-hook
                    (lambda ()
                      (setq ssh-directory-tracking-mode t)
                      (shell-dirtrack-mode t)
                      (setq dirtrackp nil))))

(req-package tramp
  :init (setq tramp-default-method "ssh"))

(req-package browse-url
  :require eww
  :init (setq browse-url-function #'eww-browse-url))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(req-package smerge-mode
  :commands smerge-mode
  :init (progn
          (defun sm-try-smerge ()
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^<<<<<<< " nil t)
                (smerge-mode 1))))
          (add-hook 'find-file-hook 'sm-try-smerge t)))

;;;; Elisp Libraries
(req-package alert
  :init (setq alert-default-style 'notifier))

(req-package-finish)
(load custom-file)
(load-theme 'fogus t)
