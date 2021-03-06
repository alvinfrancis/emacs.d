;;;; gc init threshold
(setq gc-cons-threshold most-positive-fixnum)
(run-with-idle-timer
 5 nil
 (lambda ()
   (setq gc-cons-threshold 800000)))

;;;; package
(require 'package)

(setq
 package-user-dir (expand-file-name (file-name-as-directory "elpa") user-emacs-directory)
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

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
(if (display-graphic-p)
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


;; others
(setq
 auto-save-default nil
 create-lockfiles nil
 make-backup-files nil
 visible-bell t                         ; prefer visual over beeping
 fill-column 80
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)
 auto-window-vscroll nil                ; speed up cursor movement
 )

;;;; Defaults
(setq-default
 indent-tabs-mode nil         ; whitespace > tabs
 tab-width 4                  ;
 truncate-lines t             ; do not wrap by default
 word-wrap t                  ; but turn on word wrap if line wrapping
 )

;; In OSX by default, Braille Unicode falls back to the Apple Braille
;; font.  Apple Braille is bugged, so we fall back to Apple Symbols
;; instead.
(when (featurep 'mac)
  (set-fontset-font "fontset-default"
                    '(#x2800 . #x28ff)
                    "Apple Symbols"))

(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defadvice load-theme (after hide-vertical-border activate)
  (set-face-attribute 'vertical-border nil :foreground
                      (face-attribute 'default :background)))

(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
(setq inferior-lisp-program "/opt/local/bin/sbcl")

(cl-labels ((help-prefix (key command)
                         (define-key 'help-command key command)))
  (help-prefix (kbd "C-f") 'find-function)
  (help-prefix (kbd "C-k") 'find-function-on-key)
  (help-prefix (kbd "C-l") 'find-library)
  (help-prefix (kbd "C-v") 'find-variable))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enabled disabled commands

(put 'narrow-to-region 'disabled nil)

;; --------------

(use-package bind-key
  :config (bind-key "<f9>" 'fullscreen-toggle))

(use-package linum
  :init (progn
          (setq linum-format " %d ")
          (add-hook 'prog-mode-hook 'linum-mode)))

(use-package linum-relative
  :commands (linum-relative-mode linum-relative-toggle)
  :init (progn
          (setq linum-relative-format " %3s ")
          (add-hook 'prog-mode-hook 'linum-relative-mode)))

(use-package paren
  :defer 60
  :config (show-paren-mode t))

(use-package ido
  :init (setq ido-enable-flex-matching t
              ido-everywhere t)
  :config (ido-mode 1))

(use-package recentf
  :demand t
  :init (progn
          (setq
           recentf-mode t
           recentf-max-saved-items 500)))

(use-package rainbow-delimiters
  :init (progn
          (setq rainbow-delimiters-max-face-count 1)
          (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
  :commands rainbow-delimiters-mode
  :config (progn
            (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                                :foreground 'unspecified
                                :inherit 'error)))

(use-package undo-tree
  :defer t
  :init (progn
          (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.undo")))
          (setq undo-tree-auto-save-history t)))

(use-package eldoc
  :defer t
  :init (progn
          (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
          (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
          (add-hook 'ielm-mode-hook 'eldoc-mode)
          (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode)))

(use-package adaptive-wrap
  :init (setq-default adaptive-wrap-extra-indent 2)
  :config (progn
            (when (fboundp 'adaptive-wrap-prefix-mode)
              (defun visual-line-adaptive-wrap-prefix-mode ()
                (adaptive-wrap-prefix-mode visual-line-mode)))
            ;; (add-hook
            ;;  'visual-line-mode-hook
            ;; 'visual-line-adaptive-wrap-prefix-mode)
 ))

(use-package git-gutter
  :if (display-graphic-p)
  :defer 10)

(use-package git-gutter-fringe
  :if (display-graphic-p)
  :defer 10
  :init (setq-default fringes-outside-margins t)
  :config (progn
            (define-fringe-bitmap 'git-gutter-fr:added
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:modified
              [3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3]
              nil nil 'center)
            (define-fringe-bitmap 'git-gutter-fr:deleted
              [3 3 3 3 3 3 3 3 3 3 3 3 3 31 15 7 3 1]
              nil nil 'bottom)

            (global-git-gutter-mode)))

(use-package git-gutter+
  :defer 10
  :if (not (display-graphic-p))
  :config (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :disabled t
  :defer 10
  :if (display-graphic-p)
  :config (global-git-gutter+-mode t))

(use-package key-chord
  :init (setq key-chord-one-key-delay 0.5
              key-chord-two-keys-delay 0.5)
  :config (progn
            (key-chord-mode t)))

(use-package evil
  :demand t
  :init (setq evil-move-cursor-back nil
              evil-search-module 'evil-search
              evil-want-fine-undo nil
              evil-want-C-u-scroll t
              evil-want-C-d-scroll t
              evil-want-Y-yank-to-eol t
              evil-ex-search-persistent-highlight nil)
  :bind (:map evil-normal-state-map
              (";" . evil-ex)
              ("<left>" . evil-prev-buffer)
              ("<right>" . evil-next-buffer)
              ("g c" . evil-toggle-comment)
              (", u" . universal-argument)
              (", x p" . eval-print-last-sexp)
              (", x e" . eval-last-sexp)
              (", x x" . eval-defun))
  :bind (:map evil-visual-state-map
              (";" . evil-ex)
              (", u" . universal-argument))
  :bind (:map evil-motion-state-map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("-" . evil-jump-up)
              ("SPC" . evil-jump-down)
              (":" . evil-repeat-find-char))
  :config (progn
            (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
            (key-chord-define evil-replace-state-map "jj" 'evil-normal-state)

            (evil-define-motion evil-ace-jump-char-mode (count)
              :type exclusive
              (ace-jump-mode 5)
              (recursive-edit))
            (evil-define-motion evil-jump-up (count)
              (evil-previous-visual-line (* (or count 1) 5)))
            (evil-define-motion evil-jump-down (count)
              (evil-next-visual-line (* (or count 1) 5)))
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

            (evil-mode t))
  )

(use-package ace-jump-mode
  :bind (:map evil-motion-state-map
              ("s-f" . evil-ace-jump-char-mode))
  :config (add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit))

(use-package evil-surround
  :config (progn
            (evil-define-key 'visual evil-surround-mode-map
              "s" nil
              "S" 'evil-surround-region)
            (global-evil-surround-mode)))

(use-package evil-visualstar
  ;; Problems with using n and N to continue the search
  :config (global-evil-visualstar-mode))

(use-package evil-indent-textobject)

(use-package avy)

(use-package evil-easymotion
  :init (setq evilem-keys (list ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m
                                ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z
                                ?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M
                                ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z))
  :config (progn
            (let ((prefix "M-SPC"))
              (evilem-default-keybindings prefix)
              ;; Override built-ins
              ;; (evilem-define (kbd (concat prefix " w")) #'evil-forward-word-begin)
              (evilem-define (kbd (concat prefix " W")) #'evil-forward-WORD-begin
                             :scope 'paragraph)
              ;; (evilem-define (kbd (concat prefix " e")) #'evil-forward-word-end)
              (evilem-define (kbd (concat prefix " E")) #'evil-forward-WORD-end
                             :scope 'paragraph)
              ;; (evilem-define (kbd (concat prefix " b")) #'evil-backward-word-begin)
              (evilem-define (kbd (concat prefix " B")) #'evil-backward-WORD-begin
                             :scope 'paragraph)
              ;; (evilem-define (kbd (concat prefix " ge")) #'evil-backward-word-end)
              (evilem-define (kbd (concat prefix " gE")) #'evil-backward-WORD-end
                             :scope 'paragraph)
              )))

(use-package evil-multiedit
  :demand t
  :bind (:map evil-normal-state-map
              ("C-;" . evil-multiedit-match-all)
              (", m" . evil-multiedit-match-and-next))
  :bind (:map evil-visual-state-map
              (", m" . evil-multiedit-match-and-next))
  :bind (:map evil-multiedit-state-map
              ("g x" . evil-multiedit-toggle-or-restrict-region)
              ("C-;" . evil-multiedit-abort)
              ("C-n" . evil-multiedit-match-and-next)
              ("C-p" . evil-multiedit-match-and-prev)
              ("g n" . evil-multiedit-next)
              ("g p" . evil-multiedit-prev))
  :config (key-chord-define evil-multiedit-insert-state-map "jj" 'evil-multiedit-state))

(use-package evil-anzu
  :init (setq anzu-cons-mode-line-p nil)
  :defer 10)

(use-package hippie-exp
  :disabled t
  :bind (:map evil-insert-state-map
              ("M-TAB" . hippie-expand)))

(use-package company
  :init (setq company-idle-delay nil)
  :bind (:map company-mode-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :bind (:map evil-insert-state-map
              ("M-TAB" . company-indent-or-complete-common))
  :demand t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode))

(use-package elscreen
  :demand t
  :init (progn
          (setq elscreen-tab-display-control nil
                elscreen-display-tab nil
                elscreen-display-screen-number nil
                elscreen-tab-display-kill-screen nil)

          (defun evil-elscreen-quit ()
            (interactive)
            (if (and (not (elscreen-one-screen-p))
                     (one-window-p))
                (elscreen-kill)
              (evil-quit)))

          (evil-ex-define-cmd "quit" #'evil-elscreen-quit))
  :bind (:map evil-normal-state-map
              (", n n" . elscreen-create)
              (", t c" . elscreen-kill)
              (", t T" . elscreen-toggle-display-tab)
              ("H" . elscreen-previous)
              ("L" . elscreen-next))
  :config (progn
            (elscreen-start)))

(use-package powerline
  :init (setf powerline-default-separator nil)
  :config (progn
            (defadvice load-theme (after reset-powerline activate)
              (powerline-reset))
            (powerline-center-evil-theme)))

(use-package highlight
  :bind (:map evil-normal-state-map
              (", h h" . hlt-highlight-symbol)
              (", h x" . hlt-unhighlight-symbol)))

(use-package org
  :init (progn
          (setq org-fontify-quote-and-verse-blocks t
                org-src-fontify-natively t
                org-tags-exclude-from-inheritance (quote ("crypt"))
                org-crypt-key "alvin.francis.dumalus@gmail.com"
                org-hide-emphasis-markers t
                org-adapt-indentation nil
                org-imenu-depth 3)
          (evil-define-key 'normal org-mode-map
            (kbd "> >") 'org-indent-item
            (kbd "> t") 'org-indent-item-tree
            (kbd "< <") 'org-outdent-item
            (kbd "< t") 'org-outdent-item-tree)
          ;; Org-Capture
          (setq org-directory "~/Documents/org")
          (setq org-default-notes-file (concat org-directory "/notes.org"))
          (setq org-capture-templates
                '(("n" "Notes" entry (file+headline (concat org-directory "/notes.org") "Notes")
                   "* %U\n %i\n%?"))))
  :bind (:map evil-normal-state-map
              (", , c" . org-capture))
  :bind (:map evil-visual-state-map
              (", , c" . org-capture))
  :config (progn
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((emacs-lisp . t)
               (sql . t)
               (js . t)
               (ipython . t)
               (python . t)
               (clojure . t)))
            (setq org-babel-clojure-backend 'cider)
            (require 'org-crypt)
            (org-crypt-use-before-save-magic)))

(use-package org-journal
  :defer t
  :init (progn
          (setq org-journal-file-format "%Y-%m-%d.org")
          (add-hook 'org-journal-mode-hook 'auto-fill-mode)))

(use-package org-page
  :defer t
  :init (setq op/repository-directory "~/Github/alvinfrancis.github.io"
              op/site-domain "https://alvinfrancis.github.io"
              op/theme 'mdo))

(use-package ob-ipython
  :defer t
  :init (setq ob-ipython-command "ipython"))

(use-package paredit
  :defer t
  :init (progn
          (dolist (hook '(emacs-lisp-mode-hook
                          cider-repl-mode-hook
                          eval-expression-minibuffer-setup-hook
                          clojure-mode-hook
                          lisp-mode-hook
                          lisp-interaction-mode-hook
                          scheme-mode-hook))
            (add-hook hook 'enable-paredit-mode))
          (evil-define-key 'normal paredit-mode-map
            (kbd "< h") 'paredit-backward-slurp-sexp
            (kbd "> l") 'paredit-forward-slurp-sexp
            (kbd "> h") 'paredit-backward-barf-sexp
            (kbd "< l") 'paredit-forward-barf-sexp
            ;; NOTE: Inteferes with d-prefix keys
            ;; (kbd "d s f") 'paredit-splice-sexp
            )))

(use-package slime
  :init (progn
          (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
          (evil-define-key 'normal slime-mode-map
            (kbd ", x e") 'slime-eval-last-expression
            (kbd ", x x") 'slime-eval-defun
            (kbd ", x p") 'slime-pprint-eval-last-expression))
  :commands slime
  :config (slime-setup '(slime-fancy slime-asdf)))

(use-package trident-mode
  :mode ("\\.paren\\'" . lisp-mode)
  :commands trident-mode
  :init (progn
          (add-hook 'lisp-mode-hook
                    (lambda ()
                      (when (and buffer-file-name
                                 (string-match-p "\\.paren\\>" buffer-file-name))
                        (unless (slime-connected-p)
                          (save-excursion (slime)))
                        (trident-mode +1))))
          (add-hook 'trident-mode-hook
                    (lambda () (trident-add-keys-with-prefix "C-c C-e")))))

(use-package clojure-mode
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
            (put-clojure-indent 'defui '(:defn (1)))
            (evil-define-key 'normal clojure-mode-map
              (kbd "C-c C-e") 'lisp-eval-last-sexp
              (kbd "C-c C-c") 'lisp-eval-defun)
            (evil-define-key 'visual clojure-mode-map
              (kbd "C-c C-r") 'lisp-eval-region)))

(use-package cider
  :pin melpa-stable
  :commands cider-mode
  :init (progn
          (setq cider-repl-display-help-banner nil
                cider-boot-parameters "cider repl -s wait")
          (evil-define-key 'normal cider-mode-map
            (kbd ", x p") 'cider-eval-print-last-sexp
            (kbd ", x e") 'cider-eval-last-sexp
            (kbd ", x x") 'cider-eval-defun-at-point
            (kbd "C-]") 'cider-find-var)
          (evil-define-key 'visual cider-mode-map
            (kbd ", x r") 'cider-eval-region)
          (evil-define-key 'normal cider-popup-buffer-mode-map
            "q" 'cider-popup-buffer-quit-function)
          (evil-define-key 'normal cider-stacktrace-mode-map
            "q" 'cider-popup-buffer-quit-function)
          (evil-define-key 'normal cider-docview-mode-map
            "q" 'cider-popup-buffer-quit-function)
          (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel! \"main\" \"devcards\") (cljs-repl))")
          (add-hook 'cider-mode-hook 'eldoc-mode)
          (add-hook 'cider-mode-hook 'cider-eldoc-setup)))

(use-package inf-clojure
  :commands inf-clojure)

(use-package python
  :defer t
  :init (setq python-shell-completion-native-enable nil))

(use-package scala-mode
  :disabled t
  :commands scala-mode
  :init (add-hook 'scala-mode-hook
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
                          ))))

(use-package web-mode
  :commands web-mode
  :init (progn
          (setq web-mode-code-indent-offset 2)
          (evil-define-key 'operator web-mode-map
            (kbd "g c") 'web-mode-comment-or-uncomment)))

(use-package csharp-mode
  :commands csharp-mode
  :config (progn
            (electric-indent-mode t)
            (add-hook 'csharp-mode-hook
                      (lambda ()
                        (electric-pair-mode)
                        (setq electric-pair-pairs '((?\" . ?\")
                                                    (?\{ . ?\})
                                                    (?\< . ?\>)))))))

(use-package vimrc-mode
  :commands vimrc-mode
  :mode (".vim\\(rc\\)?$" . vimrc-mode))

(use-package js2-mode
  :commands js2-mode)

(use-package jsx-mode
  :init (setf jsx-indent-level 2)
  :commands jsx-mode
  :mode ("\\.jsx\\'" . jsx-mode))

(use-package sql
  :defer t
  :init (progn
          (setq sql-product 'postgres
                sql-server "localhost")
          (add-hook 'sql-interactive-mode-hook
                    (lambda ()
                      (unbind-key ";" sql-interactive-mode-map))))

  :config
  (bind-keys
   :map evil-normal-state-map
   (", b" . sql-beautify-paragraph)
   :map evil-visual-state-map
   (", b" . sql-beautify-region))
  (defun sql-beautify-region (beg end)
    "Beautify SQL in region between BEG and END."
    (interactive "r")
    (save-excursion
      (shell-command-on-region
       beg end
       "sqlformat-2.7 --reindent --keyword=\"upper\" -" nil t)))

  (defun sql-beautify-paragraph ()
    "Beautify contiguous SQL code under point."
    (interactive)
    (let ((start (save-excursion
                   (backward-paragraph)
                   (point)))
          (end (save-excursion
                 (forward-paragraph)
                 (point))))
      (sql-beautify-region start end)))

  (defun sql-send-paragraph ()
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

(use-package sgml-mode
  :commands html-mode
  :init (cl-labels ((local-vars ()
                                (setq-local tab-width 2)))
          (add-hook 'html-mode-hook #'local-vars)))

(use-package yaml-mode
  :commands yaml-mode)

(use-package go-mode
  :defer t
  :init (progn
          (cl-labels ((go-mode-setup ()
                                     (setq compile-command "go build -v && go test -v")
                                     (go-eldoc-setup)
                                     (flycheck-mode)
                                     (set (make-local-variable 'company-backends) '(company-go))
                                     (add-hook 'before-save-hook #'gofmt-before-save nil 'local)))
            (add-hook 'go-mode-hook #'go-mode-setup)))
  :bind (:map go-mode-map
              ("C-c C-c" . compile)
              ("C-c C-d" . godoc-at-point)))

(use-package go-eldoc
  :defer t)

(use-package company-go
  :init (setq company-go-gocode-command "gocode")
  :defer t)

(use-package magit
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (evil-ex-define-cmd "Gstatus" #'magit-status)
          (evil-ex-define-cmd "Gs" "Gstatus")
          (add-hook 'magit-mode-hook
                    (lambda ()
                      (hl-line-mode))))
  :bind ("<f10>" . magit-status))

(use-package git-timemachine
  :commands git-timemachine-toggle
  :init (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
  :config (evil-make-overriding-map git-timemachine-mode-map 'normal))

(use-package smerge-mode
  :commands smerge-mode
  :bind (:map evil-normal-state-map
              (", g j" . smerge-next)
              (", g k" . smerge-prev)
              (", g RET" . smerge-keep-current))
  :init (progn
          (defun sm-try-smerge ()
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^<<<<<<< " nil t)
                (smerge-mode 1))))
          (add-hook 'find-file-hook 'sm-try-smerge t)))

(use-package projectile
  :defer t)

(use-package helm
  :pin melpa-stable
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         :map evil-visual-state-map
         (", ;" . helm-M-x)
         :map evil-normal-state-map
         (", ;" . helm-M-x)
         ("C-p" . helm-files-mru)
         ("\\ b" . helm-buffers-list)
         ("\\ y" . helm-show-kill-ring)
         ("\\ m" . helm-imenu)
         ("\\ \\" . helm-resume))
  :config (progn
            (require 'helm-for-files)
            (defun helm-files-mru ()
              (interactive)
              (helm :sources '(helm-source-recentf
                               helm-source-find-files)
                    :buffer "*helm-files-mru-buffers*"))
            (helm-mode t)))

(use-package helm-ls-git
  :defer t
  :bind (:map evil-normal-state-map
              ("\\ f" . helm-browse-project)))

(use-package helm-swoop
  :defer t
  :bind (:map evil-normal-state-map
              ("\\ l" . helm-swoop)))

(use-package helm-descbinds
  :defer t)

(use-package helm-ag
  :defer t)

(use-package helm-projectile
  :defer t
  :bind (:map evil-normal-state-map
              ("\\ p p" . helm-projectile)
              ("\\ p a" . helm-projectile-ag)))

(use-package restclient
  :commands restclient-mode)

(use-package package
  :init (cl-labels ((package-menu-evil ()
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
          (add-hook 'package-menu-mode-hook #'package-menu-evil)))

(use-package which-func
  :bind ("C-x 4 n" . clone-buffer-and-narrow-to-function)
  :commands which-function
  :init (defun clone-buffer-and-narrow-to-function ()
          (interactive)
          (clone-indirect-buffer-other-window (which-function) 'pop-to-buffer)
          (mark-defun) ; works not only in emacs-lisp, but C++, Python, ...
          (narrow-to-region (mark) (point))
          (pop-mark)
          (other-window 1)))

(use-package exec-path-from-shell
  :demand t
  :config (exec-path-from-shell-initialize))

(use-package ssh
  :commands (ssh)
  :init (add-hook 'ssh-mode-hook
                  (lambda ()
                    (setq ssh-directory-tracking-mode t)
                    (shell-dirtrack-mode t)
                    (setq dirtrackp nil))))

(use-package tramp
  :defer t
  :functions (tramp-read-passwd)
  :init (setq tramp-default-method "ssh"))

(use-package eww
  :defer t)

(use-package browse-url
  :init (setq browse-url-function #'eww-browse-url))

(use-package alert
  :defer t
  :init (setq alert-default-style 'notifier))

(use-package lively
  :commands lively)

(use-package esup
  :commands esup)

(use-package rainbow-mode
  :defer t)

(use-package ranger
  :defer t)

(use-package flycheck
  :defer t
  :init (setq flycheck-indication-mode 'right-fringe)
  :config (progn
            (when (fboundp 'define-fringe-bitmap)
              (define-fringe-bitmap 'flycheck-fringe:error
                (vector #b11000011
                        #b01100110
                        #b00111100
                        #b00011000
                        #b00111100
                        #b01100110
                        #b11000011))
              (define-fringe-bitmap 'flycheck-fringe:warning
                (vector #b00011000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00000000
                        #b00000000
                        #b00011000
                        #b00011000))
              (define-fringe-bitmap 'flycheck-fringe:warning
                (vector #b00011000
                        #b00011000
                        #b00000000
                        #b00000000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00011000
                        #b00011000)))
            (flycheck-define-error-level 'error
              :severity 100
              :compilation-level 2
              :overlay-category 'flycheck-error-overlay
              :fringe-bitmap 'flycheck-fringe:error
              :fringe-face 'flycheck-fringe-error
              :error-list-face 'flycheck-error-list-error)
            (flycheck-define-error-level 'warning
              :severity 10
              :compilation-level 1
              :overlay-category 'flycheck-warning-overlay
              :fringe-bitmap 'flycheck-fringe:warning
              :fringe-face 'flycheck-fringe-warning
              :error-list-face 'flycheck-error-list-warning)
            (flycheck-define-error-level 'info
              :severity -10
              :compilation-level 0
              :overlay-category 'flycheck-info-overlay
              :fringe-bitmap 'flycheck-fringe:info
              :fringe-face 'flycheck-fringe-info
              :error-list-face 'flycheck-error-list-info)))

(use-package flycheck-package
  :defer t
  :config (flycheck-package-setup))

(use-package vdiff
  :defer t
  :config (progn
            (set-face-attribute 'diff-changed nil :background "purple")))

(use-package org-present
  :bind (:map evil-normal-state-map
              (", l" . org-present-next)
              (", h" . org-present-prev)))

(use-package elfeed
  :pin melpa-stable
  :defer t)

(use-package request
  :pin melpa-stable
  :defer t)

(use-package drawille
  :disabled t
  :defer t)

(use-package spark
  :defer t)

(load custom-file)

(use-package gruvbox-theme
  :pin melpa-stable
  :config (load-theme 'gruvbox))

(use-package threads-modeline
  :demand t
  :init (setf mode-line-bar-color "RoyalBlue")
  :load-path "lisp/")
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
