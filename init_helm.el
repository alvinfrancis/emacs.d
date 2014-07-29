(helm-mode t)
(require 'helm-ls-git)

;;;; sources and keymaps
(defun helm-files-mru ()
  (interactive)
  (helm :sources '(helm-source-recentf
                   helm-source-findutils
                   helm-source-files-in-current-dir)
        :buffer "*helm-files-mru-buffers*"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key evil-normal-state-map
  (kbd "C-p") 'helm-files-mru)
(define-key evil-normal-state-map
  (kbd "\\ f") 'helm-browse-project)  ;; C-] to toggle pathnames
(define-key evil-normal-state-map
  (kbd "\\ b") 'helm-buffers-list)
(define-key evil-normal-state-map
  (kbd "\\ l") 'helm-swoop)
(define-key evil-normal-state-map
  (kbd "\\ \\") 'helm-resume)

(setq recentf-max-saved-items 500)
