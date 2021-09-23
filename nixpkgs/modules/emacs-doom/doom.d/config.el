;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ================= USER =================

(setq user-full-name "Rafael Varago"
      user-mail-address "rafael.varago@gmail.com")

;; ================= UI =================

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-zenburn
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec :family "JetBrains Mono" :size 15)
      doom-big-font (font-spec :family "JetBrains Mono" :size 30)
      display-line-numbers-type t)

(defun my/protect-buffers ()
  "Protects some buffers from being killed."
  (let ((protected-buffers '("*scratch*" "*Messages*")))
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill)))))
  
(add-hook 'after-init-hook #'my/protect-buffers)

(use-package! auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode))

(use-package! centaur-tabs
  :bind ("C-<prior>" . centaur-tabs-backward)
        ("C-<next>" . centaur-tabs-forward))

(use-package! treemacs
  :after projectile
  :init (add-hook 'projectile-after-switch-project-hook #'treemacs-display-current-project-exclusively)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  :bind ("M-0" . treemacs-select-window))

(use-package! window
  :bind (("C-x 2" . my/vsplit-last-buffer)
         ("C-x 3" . my/hsplit-last-buffer)
         ;; Kill a buffer without asking.
         ([remap kill-buffer] . kill-this-buffer))
  :preface
  (defun my/hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun my/vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1)))

;; ================= EDIT =================

(global-set-key (kbd "<mouse-8>") 'xref-pop-marker-stack)
(global-set-key (kbd "C-c d") 'delete-region)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(setq org-directory "~/org/")

(use-package! goto-addr
  :bind (:map goto-address-highlight-keymap
          ("C-c C-o" . goto-address-at-point))
  :hook
  ((compilation-mode shell-mode eshell-mode vterm-mode) . goto-address-mode)
  (prog-mode . goto-address-prog-mode))

(use-package! hl-todo
  :config (add-to-list 'hl-todo-keyword-faces '("WIP" . "#94bff3")))

(use-package! move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; ================= PROJECT =================

(use-package! projectile
  :config (setq projectile-project-search-path '(("~/Projects/" . 3))
                projectile-switch-project-action #'projectile-dired))

;; ================= COMPLETION =================

(use-package! company
  :config (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-require-match nil)
  :bind (:map company-mode-map
          ("C-." . company-complete)))

(use-package! company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-show-single-candidate t
        company-box-max-candidates 50)

  (defadvice! +company-box-detect-deleted-frame-a (frame)
    :filter-return #'company-box--get-frame
    (if (frame-live-p frame) frame))
  (defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
    :before #'company-box-doc
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame))))
                  
(use-package! company-yasnippet
  :after (company yasnippet)
  :bind ("M-/" . company-yasnippet))

;; =================== VCS ===================

(use-package! magit
  :bind
  ("C-c v m" . magit-checkout)
  ("C-c v e" . magit-ediff-resolve)
  ("C-c v P" . magit-push))

;; ================= CHECKER =================

(use-package! flycheck
  :preface

  (defun my/flycheck-switch-to-list-errors ()
  "Switches to flycheck list errors window."
  (interactive)
  (flycheck-list-errors)
  (pop-to-buffer "*Flycheck errors*"))
   
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  
  :bind (:map flycheck-mode-map
         ("C-c x" . my/flycheck-switch-to-list-errors)))

;; ================= LSP =================

(use-package! lsp
  :init
  ;; FIXME: Somwehow ccls' lens show up in other modes.
  (remove-hook 'lsp-lens-mode-hook 'ccls-code-lens-mode)
  (add-hook 'lsp-mode-hook 'lsp-lens-mode)
  :config (setq lsp-lens-enable t))
  
;; ================= DEV =================

;; C/C++.
(use-package! cpp-auto-include
  :bind (:map c++-mode-map
          ("C-c c o" . cpp-auto-include)))

(use-package! cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :init (add-hook 'cmake-local-vars-hook #'lsp!))

(use-package! cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package! cmake-ide
  :after (cmake-mode projectile)
  :init (cmake-ide-setup)
  :hook ((cmake-mode c-mode c++-mode) . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (with-eval-after-load 'projectile
      (setq cmake-ide-project-dir (projectile-project-root))
            cmake-ide-build-dir (concat cmake-ide-project-dir "build")
            cmake-ide-compile-command
              (concat "cmake -B " cmake-ide-build-dir " && cmake --build " cmake-ide-build-dir))
    (cmake-ide-load-db))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
    (:map cmake-mode-map
      ("C-c C-c C-c" . cmake-ide-compile))
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(use-package! flycheck-clang-tidy
  :after flycheck
  :config
  (setq flycheck-clang-tidy-extra-options
        ;; Available checks: https://clang.llvm.org/extra/clang-tidy/.
        (concat "--checks=-*,clang-analyzer-*,cppcoreguidelines-*,google-*"))
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  ((c-mode c++-mode) . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (c/c++-clang-tidy)))))))))

(use-package! google-c-style
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))


;; Docker
(use-package! dockerfile-mode
  :hook
  (dockerfile-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (dockerfile-hadolint)))))))))


;; Go.
(use-package! flycheck-golangci-lint
  :after flycheck
  :config
  ;; Enable this if things slow down significantly.
  ;; (setq flycheck-golangci-lint-fast t)
  :hook
  (flycheck-mode . flycheck-golangci-lint-setup)
  (go-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))

(use-package! dap-go
  :after (dap-mode go-mode)
  :config (dap-go-setup))


;; Graphviz.
(use-package! graphviz-dot-mode
  :config (setq graphviz-dot-indent-width 4))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)

;; Haskell.
(use-package! haskell-cabal
  :mode ("\\.cabal\\'" . haskell-cabal-mode)
  :bind (:map haskell-cabal-mode-map
          ("C-c C-c C-c" . haskell-process-cabal-build)))


;; Ini.
(use-package! ini-mode
  :mode "\\.ini\\'")


;; Java.
(use-package! gradle-mode
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


;; Kubernetes.
(use-package! kubernetes
  :commands (kubernetes-overview)
  :config (setq kubernetes-poll-frequency 3600
                kubernetes-redraw-frequency 3600))


;; Markdown.
(use-package! markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'")
  :init (add-hook 'markdown-mode-local-vars-hook #'lsp!)
  :hook (markdown-mode . pandoc-mode)
  :custom (markdown-command "pandoc"))

(use-package! markdown-preview-mode
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

(use-package! pandoc-mode
  :defer
  :bind (:map markdown-mode-map
              ("C-c C-c C-c" . pandoc-run-pandoc)))


;; Nix.
(use-package! nix-mode
  :init (add-hook 'nix-mode-local-vars-hook #'lsp!))


;; Protobuf.
(use-package! protobuf-mode)


;; Python.
(use-package! lsp-pyright
  :config (setq lsp-pyright-typechecking-mode "basic")
  :hook
  (python-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))))


;; Rust.
(use-package! rustic
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t)
  :bind (:map rustic-mode-map
          ("C-c C-c C-t" . rustic-cargo-test-rerun)
          ("C-c C-c C-d" . rustic-cargo-doc)
          ("C-c C-c C-a" . rustic-cargo-add)
          ("C-c C-c C-e" . lsp-rust-analyzer-expand-macro)))

(use-package! dap-cpptools
  :after (dap-mode rustic)
  :config (dap-cpptools-setup))

;; Scala.
(use-package! lsp-metals
  :config (setq lsp-metals-show-inferred-type t))


;; Shell.
(use-package! sh-script
  :hook
  (sh-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sh-shellcheck)))))))))


;; Sql.
(use-package! sql
  :after flycheck
  :init (add-hook 'sql-mode-local-vars-hook #'lsp!)
  :hook
  (sql-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sql-sqllint))))))))
  :config
  ;; Fixes default regexp with mysql/mariadb.
  (sql-set-product-feature 'mysql :prompt-regexp "^\\(MariaDB\\|MySQL\\) \\[[_a-zA-Z]*\\]> ")
  (unbind-key "C-c C-c" sql-mode-map)
  :bind (:map sql-mode-map
          ("C-c C-c C-p" . lsp-sql-execute-paragraph)
          ("C-c C-c C-r" . lsp-sql-execute-query)
          ("C-c C-c C-c" . lsp-sql-switch-connection)
          ("C-c C-c C-d" . lsp-sql-switch-database)
          ("C-c C-c C-l" . lsp-sql-show-connections)
          ("C-c C-c C-d" . lsp-sql-show-databases)
          ("C-c C-c C-s" . lsp-sql-show-schemas)))

(use-package! sqlformat
  :after sql
  :config (setq sqlformat-command 'sqlfluff)
  :bind (:map sql-mode-map
          ("C-c c f" . 'sqlformat)))

(use-package! sqlup-mode
  :after sql
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  :bind (:map sql-mode-map
          ("C-c c u r" . sqlup-capitalize-keyworks-in-region)
          ("C-c c u b" . sqlup-capitalize-keyworks-in-buffer)))


;; Systemd.
(use-package! systemd)


;; Yang.
(use-package! yang-mode)


;; Xml.
(use-package! nxml-mode
  :init (add-hook 'nxml-mode-local-vars-hook #'lsp!)
  :config (setq nxml-child-indent 2
        nxml-attribute-indent 4
        nxml-slash-auto-complete-flag t))
