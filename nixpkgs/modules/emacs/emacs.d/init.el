(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(eval-when-compile
  (require 'use-package))

(load-file "~/.emacs.d/lisp/edit.el")
(load-file "~/.emacs.d/lisp/git.el")
(load-file "~/.emacs.d/lisp/ide.el")
(load-file "~/.emacs.d/lisp/ui.el")

(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode))

(use-package direnv
  :config (direnv-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

;; - command-log-mode
;; - clm/open-command-log-buffer
(use-package command-log-mode)

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
  ;; See: https://emacs.stackexchange.com/q/10065/22105
  :init (with-eval-after-load 'python (elpy-enable))
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  ;; by default, elpy uses flymake. This forces it to use flycheck instead
  ;; See:
  ;;     - https://github.com/jorgenschaefer/elpy/wiki/Customizations#use-flycheck-instead-of-flymake
  ;;     - https://github.com/jorgenschaefer/elpy/issues/137
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package rg
  :config (rg-enable-default-bindings))

(use-package sql
  :config
  ;; with mariadb, the default regexp used to match the prompt is a bit off. This fixes it.
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> "))

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))

(use-package undo-tree
  :init (global-undo-tree-mode))
