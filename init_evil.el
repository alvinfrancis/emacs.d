(require 'evil)

(evil-mode t)

(setq evil-move-cursor-back nil)
(setq evil-search-module 'evil-search)

;; copy pasted move-key (TODO: make this better)
;; (defun move-key (keymap-from keymap-to key)
;;   "Moves key binding from one keymap to another, deleting from the old location. "
;;   (define-key keymap-to key (lookup-key keymap-from key))
;;   (define-key keymap-from key nil))
;; (move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
;; (move-key evil-motion-state-map evil-normal-state-map " ")

(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(evil-define-motion evil-jump-up (count)
  (evil-previous-line (* (or count 1) 5)))
(evil-define-motion evil-jump-down (count)
  (evil-next-line (* (or count 1) 5)))
(define-key evil-motion-state-map (kbd "-") 'evil-jump-up)
(define-key evil-motion-state-map (kbd "SPC") 'evil-jump-down)
(define-key evil-normal-state-map (kbd "SPC") nil)  ; Undefine space from the normal-state-map

;; use keychord library for key chords
(key-chord-mode t)
(setq key-chord-one-key-delay 1.0)
(setq key-chord-two-keys-delay 1.0)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jj" 'evil-normal-state)

;; Minimize chording
(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-normal-state-map (kbd ", ;") 'helm-M-x)
;; (define-key evil-motion-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ", ;") 'helm-M-x)
(define-key evil-normal-state-map (kbd ":") 'evil-repeat-find-char)
(define-key evil-motion-state-map (kbd ":") 'evil-repeat-find-char)
;; (define-key evil-visual-state-map (kbd ":") 'evil-repeat-find-char)

;; Buffer changing
(define-key evil-normal-state-map
  (kbd "<left>") 'evil-prev-buffer)
(define-key evil-normal-state-map
  (kbd "<right>") 'evil-next-buffer)

;; Fix Y
(defun evil-yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
)
(define-key evil-normal-state-map (kbd "Y") 'evil-yank-to-end-of-line)

;; Don't jump to next search
(defun evil-search-word-forward-stay ()
  (interactive)
  (evil-search-word-backward)
  (evil-search-word-forward))
(defun evil-search-word-backward-stay ()
  (interactive)
  (evil-search-word-forward)
  (evil-search-word-backward))
(define-key evil-motion-state-map
  (kbd "*") 'evil-search-word-forward-stay)
(define-key evil-motion-state-map
  (kbd "#") 'evil-search-word-backward-stay)

;; copy pasted code (TODO: understand this)
(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (ace-jump-mode 5)
  (recursive-edit))
(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; evil-nerd-commenter
(evil-define-operator evil-toggle-comment (beg end)
  "Comment operator that can work with evil-motions."
  (comment-or-uncomment-region beg end))
(define-key evil-normal-state-map (kbd "g c") 'evil-toggle-comment)

;; C-x evaluate lisp forms
(define-key evil-normal-state-map (kbd ", e p") 'eval-print-last-sexp)
(define-key evil-normal-state-map (kbd ", x e") 'eval-last-sexp)
(evil-define-key 'normal slime-mode-map (kbd ", x e") 'slime-eval-last-expression)
(define-key evil-normal-state-map (kbd ", , x") 'eval-defun)
(evil-define-key 'normal slime-mode-map (kbd ", , x") 'slime-eval-defun)
;; (slime-setup '(slime-fancy slime-asdf))

;; Completion
(define-key evil-insert-state-map (kbd "M-TAB") 'hippie-expand)

;; TODO: fix evil clipboard

;; TODO: Macro define-key multiple states

(cl-flet ((evil-nav-keybindings (state-map)
                                (evil-add-hjkl-bindings state-map 'emacs
                                  (kbd "C-f") 'evil-scroll-page-down
                                  (kbd "C-b") 'evil-scroll-page-up
                                  (kbd "C-d") 'evil-scroll-page-down
                                  (kbd "C-u") 'evil-scroll-page-up
                                  (kbd "C-w j") 'evil-window-down
                                  (kbd "C-w k") 'evil-window-up
                                  (kbd "C-w h") 'evil-window-left
                                  (kbd "C-w l") 'evil-window-right)))
  (evil-nav-keybindings package-menu-mode-map)
  (and (require 'magit)
       (evil-nav-keybindings magit-status-mode-map))
  (and (require 'debug)
       (evil-nav-keybindings debugger-mode-map)))
(cl-flet ((evil-option-keybindings
           (state-map)
           (evil-define-key 'emacs state-map (kbd ";") 'evil-ex)))
  (evil-option-keybindings package-menu-mode-map))


;;;; Disable keys
;; (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
;; (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
