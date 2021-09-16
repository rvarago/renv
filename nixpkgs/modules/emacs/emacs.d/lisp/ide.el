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
          ("M-n" . company-select-next)
          ("M-p" . company-select-previous)
          ("M-<" . company-select-first)
          ("M->" . company-select-last)))

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
  :preface
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  :init
  (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c e" . flycheck-list-errors)))

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise")))
  (flyspell-sort-corrections nil)
  (flyspell-issue-message-flag nil)
  :hook
  ((markdown-mode org-mode text-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package flyspell-correct-ivy
  :after (flyspell ivy)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

;; ======================== Parens ========================
(use-package smartparens
  :commands (smartparens-global-mode show-smartparens-global-mode)
  :init (smartparens-global-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-unmatched-face ((t (:background "dark gray" :foreground "red"))))
  (rainbow-delimiters-depth-1-face ((t (:foreground "wheat")))))

;; ======================== Rest Client ======================
(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)))

;; ======================== LSP + DAP ========================
(use-package lsp-mode
  :commands lsp
  ;; :after company flycheck
  :custom
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-lens-mode)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-lens-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (setq lsp-modeline-code-actions-segments '(count icon name))
  ;; (setq lsp-log-io t) ;; Enable for debugging only.
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

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'lsp-ivy-workspace-symbol))

(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  :bind (:map dap-mode-map
              ("C-c d d" . dap-debug))
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

;; C/C++.

(use-package ccls
  :after projectile
  :hook ((c-mode c++-mode objc-mode) . lsp)
  :config
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

(use-package dap-cpptools
  :after (lsp-mode ccls)
  :config
  (dap-cpptools-setup))

(use-package cpp-auto-include
  :bind (:map c++-mode-map ("C-c i" . cpp-auto-include)))

(use-package google-c-style
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package flycheck-clang-tidy
  :after flycheck
  :config
  (setq flycheck-clang-tidy-extra-options
        ;; Available checks: https://clang.llvm.org/extra/clang-tidy/.
        (concat "--checks=-*,clang-analyzer-*,cppcoreguidelines-*,google-*"))
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  ((c-mode c++-mode) . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (c/c++-clang-tidy)))))))))

(use-package cmake-mode
  :hook
  (cmake-mode . lsp)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-ide
  :after (cmake-mode projectile)
  :init (cmake-ide-setup)
  :hook
  ((cmake-mode c-mode c++-mode) . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
      (setq cmake-ide-build-dir (concat cmake-ide-project-dir "build")))
    (setq cmake-ide-compile-command
          (concat "cmake -B " cmake-ide-build-dir " && cmake --build " cmake-ide-build-dir))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind
  ([remap comment-region] . cmake-ide-compile)
  (:map cmake-mode-map
        ("C-c C-c" . cmake-ide-compile))
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))


;; Docker.

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  :hook
  (dockerfile-mode . lsp)
  (dockerfile-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (dockerfile-hadolint)))))))))

(use-package docker-compose-mode)


;; Elm.

(use-package elm-mode
  :hook (elm-mode . lsp))

(use-package lsp-elm
  :hook (elm-mode . lsp))


;; F#.

(use-package fsharp-mode
  :hook (fsharp-mode . lsp))


;; Go.

(use-package go-mode
  :hook
  ((go-mode . lsp)
  (before-save . lsp-format-buffer)
  (before-save . lsp-organize-imports)))

(use-package flycheck-golangci-lint
  :after flycheck
  :config
  ;; Enable this if things slow down significantly.
  ;; (setq flycheck-golangci-lint-fast t)
  :hook
  (go-mode . flycheck-golangci-lint-setup)
  (go-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))


(use-package dap-go
  :after (dap-mode go-mode)
  :config
  (dap-go-setup))

;; Graphviz.

(use-package graphviz-dot-mode
  :config
  (setq graphviz-dot-indent-width 4))

(use-package company-graphviz-dot
  :after (graphviz-dot-mode))


;; Haskell.

(use-package haskell-mode
  :hook (haskell-mode . lsp))

(use-package lsp-haskell
  :hook (
    (haskell-mode . lsp)
    (haskell-literate-mode . lsp)))

(use-package haskell-cabal
  :mode ("\\.cabal\\'" . haskell-cabal-mode)
  :bind (:map haskell-cabal-mode-map
              ("C-c C-c C-c" . haskell-process-cabal-build)))


;; INI.

(use-package ini-mode :mode "\\.ini\\'")


;; Java.

(use-package lsp-java
  :preface
  (add-hook 'java-mode-hook 'lsp)
  :bind ((:map java-mode-map
               ("C-c r o" . lsp-java-organize-imports))))

(use-package dap-java
  :after (dap-mode lsp-java))

(use-package gradle-mode
  :bind (:map gradle-mode-map
              ("C-c C-c C-c" . gradle-build)
              ("C-c C-c C-t" . gradle-test))
  :preface
  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :config
  (advice-add 'gradle-build :after #'my/switch-to-compilation-window)
  (advice-add 'gradle-test :after #'my/switch-to-compilation-window))


;; JSON.

(use-package json-mode
  :hook (json-mode . lsp))


;; Kubernetes.

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))


;; Markdown.

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'")
  :hook
  (markdown-mode . lsp)
  (markdown-mode . pandoc-mode)
  :custom (markdown-command "pandoc"))

(use-package markdown-preview-mode
  :commands markdown-preview-mode
  :custom
  (markdown-preview-javascript
   (list (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/highlight.min.js")
         "<script>
            $(document).on('mdContentChange', function() {
              $('pre code').each(function(i, block)  {
                hljs.highlightBlock(block);
              });
            });
          </script>"))
  (markdown-preview-stylesheets
   (list (concat "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/"
                 "3.0.1/github-markdown.min.css")
         (concat "https://github.com/highlightjs/highlight.js/"
                 "9.15.6/styles/github.min.css")

         "<style>
            .markdown-body {
              box-sizing: border-box;
              min-width: 200px;
              max-width: 980px;
              margin: 0 auto;
              padding: 45px;
            }

            @media (max-width: 767px) { .markdown-body { padding: 15px; } }
          </style>")))

(use-package pandoc-mode
  :after markdown-mode
  :hook markdown-mode
  :bind (:map markdown-mode-map
              ("C-c C-c" . pandoc-run-pandoc)))


;; Nix.

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (nix-mode . lsp))


;; Proof-assistants.

(use-package proof-general
  :commands (coq-mode)
  :hook ((coq-mode . yas-minor-mode))
  :init
  (setq proof-splash-enable nil))

(use-package company-coq
  :hook (coq-mode . company-coq-mode)
  (setq company-coq-live-on-the-edge t))

(use-package agda2-mode)

(use-package idris-mode)


;; Protobuf.

(use-package protobuf-mode)


;; Python.

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


;; Rust.

(use-package rustic
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-cargo-watch-command "clippy") ;; Default is "check".
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


;; Scala.

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

;; Shell.

(use-package sh-mode
  :hook
  (sh-mode . lsp)
  (sh-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sh-shellcheck)))))))))

;; SQL.

(use-package sql
  :config
  ;; Fixes default regexp with mysql/mariadb.
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")
  :hook (sql-mode . lsp)
  :bind (:map sql-mode-map
              ("C-c C-r" . lsp-sql-execute-query)
              ("C-c c" . lsp-sql-switch-connection)
              ("C-c d" . lsp-sql-switch-database)
              ("C-c C-l c" . lsp-sql-show-connections)
              ("C-c C-l s" . lsp-sql-show-schemas)
              ("C-c C-l d" . lsp-sql-show-databases)))


;; systemd.

(use-package systemd)


;; UML.

(use-package plantuml-mode
  :mode ("\\.\\(plantuml\\|puml\\)\\'")
  :config
  (setq plantuml-default-exec-mode 'executable))


;; YANG.

(use-package yang-mode)


;; YAML.

(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode)
  :hook (yaml-mode . lsp))


;; XML.

(use-package nxml-mode
  :mode "\\.xml\\'"
  :config
  (setq nxml-child-indent 2
        nxml-attribute-indent 4
        nxml-slash-auto-complete-flag t)
  :hook (nxml-mode . lsp))

;; ================================ Direnv =========================
(use-package direnv
  :config (direnv-mode))
