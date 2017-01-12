;;; threads-modeline.el

(require 's)
(require 'f)
(require 'powerline)
(require 'projectile)

;; Move `threads-fix-unicode` and `threads/project-root` to separate files.

(defun threads-fix-unicode (font &rest chars)
  "Display certain unicode characters in a specific font.
e.g. (threads-fix-unicode \"DejaVu Sans\" ?⚠ ?★ ?λ)"
  (declare (indent 1))
  (when (display-graphic-p)
    (mapc (lambda (x) (set-fontset-font
                       t (cons x x)
                       (cond ((fontp font)
                              font)
                             ((listp font)
                              (font-spec :family (car font) :size (nth 1 font)))
                             ((stringp font)
                              (font-spec :family font))
                             (t (error "FONT is an invalid type: %s" font)))))
          chars)))

;;;###autoload
(defun threads/project-root (&optional strict-p)
  "Get the path to the root of your project."
  (let (projectile-require-project-root strict-p)
    (projectile-project-root)))

;; Color helper functions
;; Shamelessly *borrowed* from solarized
(defun threads-name-to-rgb (color &optional frame)
  (mapcar (lambda (x) (/ x (float (car (color-values "#ffffff")))))
          (color-values color frame)))

(defun threads-blend (color1 color2 alpha)
  (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
         (--zip-with (+ (* alpha it) (* other (- 1 alpha)))
                     (threads-name-to-rgb color1)
                     (threads-name-to-rgb color2))))

(defun threads-darken (color alpha)
  (threads-blend color "#000000" (- 1 alpha)))

(defun threads-lighten (color alpha)
  (threads-blend color "#FFFFFF" (- 1 alpha)))

(defun threads-max-contrast (color)
  (let* ((hsl (apply #'color-rgb-to-hsl (threads-name-to-rgb color)))
         (lightness (nth 2 hsl)))
    (if (< lightness 0.5) "#FFF" "#000")))

(defvar mode-line-height 30
  "How tall the mode-line should be. This is only respected in GUI emacs.")

;; Load powerline only when uncompiled, in order to generate the xpm bitmaps for
;; the mode-line. This is the tall blue bar on the left of the mode-line.
;; NOTE Compile this file for a faster startup!
(eval-when-compile (require 'powerline))
;; FIXME Don't hardcode colors in
(defvar mode-line-bar-color (face-attribute font-lock-keyword-face :foreground))

(defvar mode-line-bar          (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 mode-line-bar-color nil)))
(defvar mode-line-inactive-bar (eval-when-compile (pl/percent-xpm mode-line-height 100 0 100 0 3 nil nil)))

;; Custom faces
(defface mode-line-is-modified nil
  "Face for mode-line modified symbol")

(defface mode-line-2 `((t (:inherit mode-line :foreground ,(threads-lighten mode-line-bar-color 0.5))))
  "The alternate color for mode-line text.")

(defface mode-line-selection `((t (:inherit mode-line :background ,(threads-lighten mode-line-bar-color 0.5) :foreground ,(threads-darken mode-line-bar-color 0.5))))
  "Face for bright segments of the mode-line.")

;; Same color as mode-line-bar
(defface mode-line-count `((t (:inherit mode-line :background ,mode-line-bar-color)))
  "Face for anzu/evil-substitute/evil-search number-of-matches display.")

(defface mode-line-focus `((t (:inherit mode-line :background ,mode-line-bar-color)))
  "Face for mode-line focus segment.")

;; Git/VCS segment faces
(defface mode-line-vcs-info '((t (:inherit warning)))
  "")
(defface mode-line-vcs-warning '((t (:inherit warning)))
  "")

;; Flycheck segment faces
(defface threads-flycheck-error '((t (:inherit error)))
  "Face for flycheck error feedback in the modeline.")
(defface threads-flycheck-warning '((t (:inherit warning)))
  "Face for flycheck warning feedback in the modeline.")


;;
;; Functions
;;

(defun threads-ml-flycheck-count (state)
  "Return flycheck information for the given error type STATE."
  (when (flycheck-has-current-errors-p state)
    (if (eq 'running flycheck-last-status-change)
        "?"
      (cdr-safe (assq state (flycheck-count-errors flycheck-current-errors))))))

;; pyenv/rbenv version segment
(defvar threads-ml-env-version-hook '()
  "Hook that runs whenever the environment version changes (e.g. rbenv/pyenv)")

(defun threads-ml|env-update ()
  (when threads-ml--env-command
    (let ((default-directory (threads/project-root)))
      (let ((s (shell-command-to-string threads-ml--env-command)))
        (setq threads-ml--env-version (if (string-match "[ \t\n\r]+\\'" s)
                                    (replace-match "" t t s)
                                  s))
        (run-hook-with-args 'threads-ml-env-version-hook threads-ml--env-version)))))

(defmacro def-version-cmd! (modes command)
  "Define a COMMAND for MODE that will set `threads-ml--env-command' when that mode is
activated, which should return the version number of the current environment. It is used
by `threads-ml|env-update' to display a version number in the modeline. For instance:

  (def-version-cmd! ruby-mode \"ruby --version | cut -d' ' -f2\")

This will display the ruby version in the modeline in ruby-mode buffers. It is cached the
first time."
  (add-hook! (focus-in find-file) 'threads-ml|env-update)
  `(add-hook! ,modes (setq threads-ml--env-command ,command)))


;;
;; Initialization
;;

;; Where (py|rb)env version strings will be stored
(defvar-local threads-ml--env-version nil)
(defvar-local threads-ml--env-command nil)

;; Make certain unicode glyphs bigger for the mode-line.
;; FIXME Replace with all-the-icons?
(threads-fix-unicode '("Monoid" 15) ?✱) ;; modified symbol
(let ((font "Monoid for Powerline"))
  (threads-fix-unicode (list font 12) ?)  ;; git symbol
  (threads-fix-unicode (list font 16) ?∄)  ;; non-existent-file symbol
  (threads-fix-unicode (list font 15) ?)) ;; read-only symbol

;; So the mode-line can keep track of "the current window"
(defvar mode-line-selected-window nil)
(defun threads|set-selected-window (&rest _)
  (let ((window (frame-selected-window)))
    (when (and (windowp window)
               (not (minibuffer-window-active-p window)))
      (setq mode-line-selected-window window))))
(add-hook 'window-configuration-change-hook #'threads|set-selected-window)
(add-hook 'focus-in-hook #'threads|set-selected-window)
(advice-add 'select-window :after 'threads|set-selected-window)
(advice-add 'select-frame  :after 'threads|set-selected-window)


;;
;; Mode-line segments
;;

(defun *buffer-path ()
  "Displays the buffer's full path relative to the project root (includes the
project root). Excludes the file basename. See `*buffer-name' for that."
  (when buffer-file-name
    (propertize
     (f-dirname
      (let ((buffer-path (file-relative-name buffer-file-name (threads/project-root)))
            (max-length (truncate (/ (window-body-width) 1.75))))
        (concat (projectile-project-name) "/"
                (if (> (length buffer-path) max-length)
                    (let ((path (reverse (split-string buffer-path "/" t)))
                          (output ""))
                      (when (and path (equal "" (car path)))
                        (setq path (cdr path)))
                      (while (and path (<= (length output) (- max-length 4)))
                        (setq output (concat (car path) "/" output))
                        (setq path (cdr path)))
                      (when path
                        (setq output (concat "../" output)))
                      (when (string-suffix-p "/" output)
                        (setq output (substring output 0 -1)))
                      output)
                  buffer-path))))
     'face (if active 'mode-line-2))))

(defun *buffer-name ()
  "The buffer's base name or id."
  ;; FIXME Don't show uniquify tags
  (s-trim-left (format-mode-line "%b")))

(defun *buffer-pwd ()
  "Displays `default-directory', for special buffers like the scratch buffer."
  (propertize
   (concat "[" (abbreviate-file-name default-directory) "]")
   'face 'mode-line-2))

(defun *buffer-state ()
  "Displays symbols representing the buffer's state
(non-existent/modified/read-only)"
  (when buffer-file-name
    (propertize
     (concat (if (not (file-exists-p buffer-file-name))
                 "∄"
               (if (buffer-modified-p) "✱"))
             (if buffer-read-only ""))
     'face 'mode-line-is-modified)))

(defun *buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (if (memq buffer-file-coding-system '(utf-8 utf-8-unix))
      ""
    (symbol-name buffer-file-coding-system)))

(defun *major-mode ()
  "The major mode, including process, environment and text-scale info."
  (concat (propertize (format-mode-line mode-name)
                      'mouse-face 'mode-line-selection
                      'help-echo (mapconcat (lambda (x) (format "%s" x)) minor-mode-list "\n"))
          (if (stringp mode-line-process) mode-line-process)
          (if threads-ml--env-version (concat " " threads-ml--env-version))
          (and (featurep 'face-remap)
               (/= text-scale-mode-amount 0)
               (format " (%+d)" text-scale-mode-amount))))

(defun *vc ()
  "Displays the current branch, colored based on its state."
  (when vc-mode
    (let ((backend (concat " " (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
          (face (let ((state (vc-state buffer-file-name)))
                  (cond ((memq state '(edited added))
                         'mode-line-vcs-info)
                        ((memq state '(removed needs-merge needs-update conflict removed unregistered))
                         'mode-line-vcs-warning)))))
      (if active
          (propertize backend 'face face)
        backend))))

(defvar-local threads--flycheck-err-cache nil "")
(defvar-local threads--flycheck-cache nil "")
(defun *flycheck ()
  "Persistent and cached flycheck indicators in the mode-line."
  (when (and (featurep 'flycheck)
             flycheck-mode
             (or flycheck-current-errors
                 (eq 'running flycheck-last-status-change)))
    (or (and (or (eq threads--flycheck-err-cache threads--flycheck-cache)
                 (memq flycheck-last-status-change '(running not-checked)))
             threads--flycheck-cache)
        (and (setq threads--flycheck-err-cache flycheck-current-errors)
             (setq threads--flycheck-cache
                   (let ((fe (threads-ml-flycheck-count 'error))
                         (fw (threads-ml-flycheck-count 'warning)))
                     (concat
                      (if fe (propertize (format " •%d " fe)
                                         'face (if active
                                                   'threads-flycheck-error
                                                 'mode-line)))
                      (if fw (propertize (format " •%d " fw)
                                         'face (if active
                                                   'threads-flycheck-warning
                                                 'mode-line))))))))))

(defun *selection-info ()
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and active (evil-visual-state-p))
    (propertize
     (let ((reg-beg (region-beginning))
           (reg-end (region-end))
           (evil (eq 'visual evil-state)))
       (let ((lines (count-lines reg-beg (min (1+ reg-end) (point-max))))
             (chars (- (1+ reg-end) reg-beg))
             (cols (1+ (abs (- (evil-column reg-end)
                               (evil-column reg-beg))))))
         (cond
          ;; rectangle selection
          ((or (bound-and-true-p rectangle-mark-mode)
               (and evil (eq 'block evil-visual-selection)))
           (format " %dx%dB " lines (if evil cols (1- cols))))
          ;; line selection
          ((or (> lines 1) (eq 'line evil-visual-selection))
           (if (and (eq evil-state 'visual) (eq evil-this-type 'line))
               (format " %dL " lines)
             (format " %dC %dL " chars lines)))
          (t (format " %dC " (if evil chars (1- chars)))))))
     'face 'mode-line-selection)))

(defun *macro-recording ()
  "Display current macro being recorded."
  (when (and active defining-kbd-macro)
    (propertize
     (format " %s ▶ " (char-to-string evil-this-macro))
     'face 'mode-line-focus)))

(make-variable-buffer-local 'anzu--state)
(defun *anzu ()
  "Show the current match number and the total number of matches. Requires anzu
to be enabled."
  (when (and (featurep 'evil-anzu) (evil-ex-hl-active-p 'evil-ex-search))
    (propertize
     (format " %s/%d%s "
             anzu--current-position anzu--total-matched
             (if anzu--overflow-p "+" ""))
     'face (if active 'mode-line-count))))

(defun *evil-substitute ()
  "Show number of :s matches in real time."
  (when (and (evil-ex-p) (evil-ex-hl-active-p 'evil-ex-substitute))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches "
                   (count-matches pattern (car range) (cdr range))
                   evil-ex-argument)
         " ... "))
     'face (if active 'mode-line-count))))

(defun *iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (boundp 'iedit-mode) iedit-mode)
    (propertize
     (let ((this-oc (let (message-log-max) (iedit-find-current-occurrence-overlay)))
           (length (or (ignore-errors (length iedit-occurrences-overlays)) 0)))
       (format
        " %s/%s "
        (save-excursion
          (unless this-oc
            (iedit-prev-occurrence)
            (setq this-oc (iedit-find-current-occurrence-overlay)))
          (if this-oc
              ;; NOTE: Not terribly reliable
              (- length (-elem-index this-oc iedit-occurrences-overlays))
            "-"))
        length))
     'face (if active 'mode-line-count))))

(defun *buffer-position ()
  "A more vim-like buffer position."
  (let ((start (window-start))
        (end (window-end))
        (pend (point-max)))
    (if (and (= start 1)
             (= end pend))
        " All"
      (cond ((= start 1) " Top")
            ((= end pend) " Bot")
            (t (format " %d%%%%" (/ end 0.01 pend)))))
    ))

(defun *evil-mode ()
  (let ((s (cond
            ((eq evil-state 'normal) " N ")
            ((eq evil-state 'visual) " V ")
            ((eq evil-state 'insert) " I ")
            ((eq evil-state 'replace) " R ")
            ((eq evil-state 'emacs) " E ")
            (evil-mode-line-tag))))
    (propertize s 'face (when active 'mode-line-2))))

(defun *cider-mode ()
  (when (eq major-mode 'cider-mode)
    (format "cider[%s] " (cider--modeline-info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun threads-mode-line (&optional id)
  `(:eval
    (let* ((active (eq (selected-window) mode-line-selected-window))
           (lhs (list (propertize " " 'display (if active mode-line-bar mode-line-inactive-bar))
                      (*flycheck)
                      (*macro-recording)
                      (*selection-info)
                      (*anzu)
                      (*evil-substitute)
                      (*iedit)
                      " "
                      (*evil-mode)
                      " "
                      (*buffer-path)
                      (*buffer-name)
                      " "
                      (*buffer-state)
                      ,(if (eq id 'scratch) '(*buffer-pwd))))
           (rhs (list " "
                      (*vc)
                      " "
                      (*major-mode)
                      " "
                      (*cider-mode)
                      (concat
                       (propertize
                        (concat " LN %l:%c ")
                        'face (if active 'mode-line-2))
                       (*buffer-position))))
           (middle (propertize
                    " " 'display `((space :align-to (- (+ right right-fringe right-margin)
                                                       ,(1+ (string-width (format-mode-line rhs)))))))))
      (list lhs middle rhs))))

(setq-default mode-line-format (threads-mode-line))

(provide 'threads-modeline)
;;; threads-modeline.el ends here
