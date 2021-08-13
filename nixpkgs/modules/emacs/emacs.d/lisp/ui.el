;; ============================ General emacs settings ============================

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;; Make *scratch* buffer blank.
(setq initial-scratch-message "")

;; Make window title the buffer name.
(setq-default frame-title-format '("%b"))

;; Display line number except for certain modes.
(global-display-line-numbers-mode t) ; requires emacs 26.
(dolist (mode '(message-buffer-mode-hook
                treemacs-mode-hook
                magit-status-mode-hook
                org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Display column number.
(column-number-mode)

;; Highlight cursor line.
(global-hl-line-mode +1)

;; Avoid littering the user's filesystem with backups.
(setq backup-by-copying t             ; Don't clobber symlinks.
      backup-directory-alist
      '((".*" . "~/.emacs.d/saves/")) ; Don't litter fs tree.
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; Use versioned backups.

;; No lockfiles.
(setq create-lockfiles nil)

;; A small horizontal split.
(defun create-terminal ()
  "Create a small terminal-like window."
  (interactive)
  (split-window-below)
  (setq current-prefix-arg '(10))
  (call-interactively 'enlarge-window))

(global-set-key (kbd "C-x t c") 'create-terminal)
;; =============================== Theme ===============================
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Colorful parenthesis.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-unmatched-face ((t (:background "dark gray" :foreground "red"))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "wheat")))))
(show-paren-mode 1)
(setq show-paren-delay 0)

(use-package all-the-icons)

;; ======================== Treemacs ========================
(use-package treemacs-all-the-icons)

(use-package treemacs
  :config
  (setq treemacs-show-cursor nil)
  (treemacs-load-theme "all-the-icons")
  :bind
  (([f9] . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-magit
  :after treemacs magit)

(use-package auto-dim-other-buffers
  :commands auto-dim-other-buffers-mode
  :diminish auto-dim-other-buffers-mode
  :init (auto-dim-other-buffers-mode))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  (setq uniquify-after-kill-buffer-p t)     ; Rename after killing uniquified.
  (setq uniquify-ignore-buffers-re "^\\*")) ; Don't muck with special buffers.

;; ======================== Ivy ========================
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; No regex starting with ^.
  (setq ivy-initial-inputs-alist nil))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

(use-package counsel
  :after ivy
  :config
  (counsel-mode)
  :bind ((:map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
