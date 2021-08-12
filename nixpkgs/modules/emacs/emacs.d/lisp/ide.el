;; ============================ IDE-like emacs ============================

(use-package company
  :commands company-tng-configure-default
  :custom
  ;; No delay.
  (company-idle-delay 0)
  ;; Number of chars before triggering completion.
  (company-minimum-prefix-length 1)

  :config
  ;; Enable in all buffers.
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

(use-package rustic
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (unbind-key "C-c C-c C-t" rustic-mode-map)
  ;; when passing custom test args with rustic-test-arguments, we need
  ;; to run rustic-cargo-test-rerun instead of rustic-cargo-test
  ;;
  ;; To pass custom test args, add this to .dir-locals.el:
  ;; ((rustic-mode . ((rustic-test-arguments . "-- --skip integration"))))
  :bind (("C-c C-c C-t" . rustic-cargo-test-rerun)))

(use-package lsp-haskell
  :hook (
    (haskell-mode . lsp)
    (haskell-literate-mode . lsp)))

(use-package elpy
  :commands elpy-enable
  ;; Only call `elpy-enable` when needed.
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  ;; Forces flycheck instead of flymake.
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package sql
  :config
  ;; Fixes default regexp with mariadb.
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> "))

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))

(use-package company-coq
  :hook (coq-mode . company-coq-mode))

(use-package yang-mode)

(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package direnv
  :config (direnv-mode))

(use-package command-log-mode)

(use-package undo-tree
  :init (global-undo-tree-mode))

(use-package rg
  :config (rg-enable-default-bindings))