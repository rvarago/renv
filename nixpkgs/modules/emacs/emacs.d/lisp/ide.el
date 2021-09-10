;;; ide.el --- Integrated development.
;;; Commentary:
;; Set up an integrated development environment.

;; ======================== Completions ========================
(use-package company
  :custom
  (company-begin-commands '(self-insert-command))
  ;; No delay.
  (company-idle-delay 0)
  ;; Number of chars before triggering completion.
  (company-minimum-prefix-length 1)
  ;; Allow to keep typing even if there's no match according to company.
  (company-require-match nil)
  (company-tooltip-align-annotations 't)
  :hook
  ((emacs-lisp-mode) . company-mode)
  :config
  (global-company-mode)
  (setq lsp-completion-provider :capf)
  :bind
  (:map company-mode-map
        ("C-." . company-complete))
  ;; Use only <M> and not <C> to navigate completions.
  (:map company-active-map
          ("C-n" . nil)
          ("C-p" . nil)
          ("M-n" . #'company-select-next)
          ("M-p" . #'company-select-previous)))

;; Completions with icons.
(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; ======================== Snippets ========================
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))

(use-package ivy-yasnippet
  :after (ivy yasnippet))

(use-package company-yasnippet
  :after (company yasnippet)
  :bind ("M-/" . company-yasnippet))

;; ======================== Checks ========================
(use-package flycheck
  :commands global-flycheck-mode
  :init
  (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c e" . flycheck-list-errors)))

;; flycheck?
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise")))
  (flyspell-sort-corrections nil)
  (flyspell-issue-message-flag nil)
  :hook
  (prog-mode . flyspell-prog-mode))

;; ======================== Parens ========================
(use-package smartparens
  :commands (smartparens-global-mode show-smartparens-global-mode)
  :init (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-unmatched-face ((t (:background "dark gray" :foreground "red"))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "wheat")))))

;; ======================== LSP + DAP ========================
(use-package lsp-mode
  :commands lsp
  :diminish lsp-mode
  ;; :after company flycheck
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-lens-mode)
  (lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-lens-enable t)
  :bind (:map lsp-mode-map
              ("C-c r r" . lsp-rename)
              ("C-c r f" . lsp-format-buffer)
              ("C-c r g" . lsp-format-region)
              ("C-c a" . lsp-execute-code-action)
              ("C-c f r" . lsp-find-references)
              ("C-c h" . lsp-describe-thing-at-point)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-show-with-cursor nil)
  :bind (:map lsp-ui-mode-map
              ("M-j" . lsp-ui-imenu)))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :after
  lsp-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (dap-auto-configure-mode)
  ;; Debug via dap-lldb.
  ;;(require 'dap-lldb)
  ;;(require 'dap-gdb-lldb)

  ;;(dap-gdb-lldb-setup)
  ;;(dap-register-debug-template
   ;; "Rust::LLDB Run Configuration"
   ;; (list :type "lldb"
   ;;       :request "launch"
   ;;       :name "LLDB::Run"
   ;;   :gdbpath "rust-lldb"
   ;;       :target nil
   ;;       :cwd nil)))
  )

;; ======================== Programming Languages ========================

(use-package rustic
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  ;; (setq rustic-format-on-save t)
  (unbind-key "C-c C-c C-t" rustic-mode-map)
  ;; when passing custom test args with rustic-test-arguments, we need
  ;; to run rustic-cargo-test-rerun instead of rustic-cargo-test
  ;;
  ;; To pass custom test args, add this to .dir-locals.el:
  ;; ((rustic-mode . ((rustic-test-arguments . "-- --skip integration"))))
  :bind (:map rustic-mode-map
              ("C-c C-c C-t" . rustic-cargo-test-rerun)
              ("C-c C-c C-e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c C-d" . rustic-cargo-doc)
              ("C-c C-c C-a" . rustic-cargo-add)))

(use-package ccls
  :config
  (setq lsp-prefer-flymake nil)
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package dap-cpptools
  :config
  (dap-cpptools-setup))

(use-package cpp-auto-include
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include)))

(use-package cmake-mode
  :hook (cmake-mode . lsp))

(use-package go-mode
  :hook ((go-mode . lsp)
         (before-save . lsp-format-buffer)
         (before-save . lsp-organize-imports)))

(use-package haskell-mode
  :hook (haskell-mode . lsp))

(use-package lsp-haskell
  :hook (
    (haskell-mode . lsp)
    (haskell-literate-mode . lsp)))

(use-package java-mode
  :hook (java-mode . lsp))

(use-package lsp-java
  :defer t
  :hook (java-mode . lsp)
         ;; (java-mode . lsp-java-lens-mode)
         ;; (java-mode . lsp-jt-lens-mode))
  :config
  :bind ((:map java-mode-map ("C-c r o" . 'lsp-java-organize-imports))))

(use-package dap-java
  :after dap-mode lsp-java)

(use-package fsharp-mode
  :hook (fsharp-mode . lsp))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :hook (scala-mode . lsp)
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

(use-package lsp-metals
  :custom
  (lsp-metals-show-inferred-type t))

(use-package python-mode
  :hook (python-mode . lsp)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))) ; or lsp
  :config
  (setq lsp-pyright-typechecking-mode "basic"))

(use-package sql
  :config
  ;; Fixes default regexp with mariadb.
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> "))

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter))

(use-package proof-general
  :commands (coq-mode)
  :hook ((coq-mode . yas-minor-mode))
  :init
  (setq proof-splash-enable nil))

(use-package company-coq
  :hook (coq-mode . company-coq-mode)
  (setq company-coq-live-on-the-edge t))

(use-package sh-mode
  :hook (sh-mode . lsp))

(use-package agda2-mode)

(use-package idris-mode)

(use-package yang-mode)

(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode))

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 4
        nxml-slash-auto-complete-flag t))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :hook (dockerfile-mode . lsp))

(use-package direnv
  :config (direnv-mode))

