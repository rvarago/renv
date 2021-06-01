;; ============================ Turn emacs into an IDE ============================

(use-package company
  :commands company-tng-configure-default
  :custom
  ;; delay to start completion
  (company-idle-delay 0)
  ;; nb of chars before triggering completion
  (company-minimum-prefix-length 1)

  :config
  ;; enable company-mode in all buffers
  (global-company-mode)

  :bind
  ;; use <C> instead of <M> to navigate completions
  (:map company-active-map
	      ("M-n" . nil)
	      ("M-p" . nil)
	      ("C-n" . #'company-select-next)
	      ("C-p" . #'company-select-previous)))

(use-package projectile
  :commands projectile-mode
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :init (counsel-projectile-mode +1))

(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  :hook
  (lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package flycheck
  :commands global-flycheck-mode
  :init
  (global-flycheck-mode))

(use-package yang-mode)
