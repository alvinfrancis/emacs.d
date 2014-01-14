(helm-mode t)

;; look and feel
;; created and saved using customization menu
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-header
   ((t
     (:background "#00629D"
      :foreground "#002b36"
      :underline nil
      :weight bold
      :height 1)))))

;; sources and keymaps
(defun helm-files-mru-buffers ()
  (interactive)
  (helm :sources '(helm-source-findutils
                   helm-source-files-in-current-dir
                   helm-source-recentf
                   helm-source-buffers-list)
        :buffer "*helm-files-mru-buffers*"))
(defun helm-mru ()
  (interactive)
  (helm :sources '(helm-source-findutils
                   helm-source-files-in-current-dir
                   helm-source-recentf
                   helm-source-buffers-list)
        :buffer "*helm-mru*"))

(define-key evil-normal-state-map
  (kbd ", C-p") 'helm-files-mru-buffers)
(define-key evil-normal-state-map
  (kbd ", , C-p") 'helm-mru)
