;; ============================ General emacs settings ============================

;; Maximize window on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-screen t)

;; Disable menu-bar and tool-bar
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
(defun split-small-window ()
  "Create a small terminal-like window."
  (interactive)
  (split-window-below)
  (setq current-prefix-arg '(10))
  (call-interactively 'enlarge-window))

(global-set-key (kbd "C-x t c") 'split-small-window)

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
  :bind ([f9] . treemacs)
        ("M-0" . treemacs-select-window))

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

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :hook
  (eshell-mode . centaur-tabs-local-mode)
  :custom
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-style "rounded")
  (centaur-tabs-height 36)
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "●")
  (centaur-tabs-buffer-groups-function centaur-tabs-projectile-buffer-groups)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-size 0.5)
  (shackle-default-alignment 'below)
  (shackle-rules
   '((magit-log-mode       :select t :inhibit-window-quit t :same t)
     ("*quickrun*"         :select t :inhibit-window-quit t :same t)
     (profiler-report-mode :select t)
     (apropos-mode         :select t :align t :size 0.3)
     (help-mode            :select t :align t :size 0.4)
     (comint-mode          :select t :align t :size 0.4)
     (grep-mode            :select t :align t)
     (rg-mode              :select t :align t)
     (ag-mode              :select t :align t)
     ("*org-roam*"         :select nil :align t :inhibit-window-quit t :same t)
     ("*Flycheck errors*"         :select t   :align t :size 10)
     ("*Backtrace*"               :select t   :align t :size 15)
     ("*ydcv*"                    :select nil :align t :size 0.4)
     ("*Shell Command Output*"    :select nil :align t :size 0.4)
     ("*Async Shell Command*"     :select nil :align t :size 0.4)
     ("*Org-Babel Error Output*"  :select nil :align t :size 0.3)
     ("*package update results*"  :select nil :align t :size 10)
     ("*Process List*"            :select t   :align t :size 0.3)
     ("*Help*"                    :select t   :align t :size 0.3)
     ("*Occur*"                   :select t   :align right)
     ("\\*ivy-occur .*\\*"        :select t   :align right :regexp t)
     ("\\*eldoc\\( for \\)?.*\\*" :select nil :align t :size 15 :regexp t)
     )))
