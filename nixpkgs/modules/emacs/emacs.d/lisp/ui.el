;; ============================ User interface settings  ============================

;; Maximize window on startup.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

;; Disable menu-bar and tool-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

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

(use-package auto-dim-other-buffers
  :commands auto-dim-other-buffers-mode
  :diminish auto-dim-other-buffers-mode
  :init (auto-dim-other-buffers-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.2))

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
