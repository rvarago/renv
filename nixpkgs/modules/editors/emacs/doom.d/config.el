;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ================= USER =================

(setq user-full-name "Rafael Varago"
      user-mail-address "rafael.varago@gmail.com")

;; ================= UI =================

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-zenburn
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec :family "Iosevka" :size 16)
      doom-big-font (font-spec :family "Iosevka" :size 30)
      doom-themes-treemacs-enable-variable-pitch nil
      display-line-numbers-type t)

(defun my/switch-doom-theme (theme-prefix)
  "Switches to a different doom theme, e.g. 'zenburn' or 'vibrant'."
  (interactive "stheme prefix: ")
  (let ((theme (intern (concat "doom-" theme-prefix))))
    (setq doom-theme theme)
    (load-theme theme)))

(map! "C-x t s" #'my/switch-doom-theme)

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-doc-face :slant italic))

(defun my/protect-buffers ()
  "Protects some buffers from being killed."
  (let ((protected-buffers '("*scratch*" "*Messages*")))
    (dolist (buffer protected-buffers)
      (with-current-buffer buffer
        (emacs-lock-mode 'kill)))))
  
(add-hook 'after-init-hook #'my/protect-buffers)

(use-package! auto-dim-other-buffers
  :defer t
  :init (auto-dim-other-buffers-mode))

(use-package! centaur-tabs
  :defer t
  :bind ("C-<prior>" . centaur-tabs-backward)
        ("C-<next>" . centaur-tabs-forward))

(use-package! treemacs
  :defer t
  :after projectile
  :init (add-hook 'projectile-after-switch-project-hook #'treemacs-display-current-project-exclusively)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  :bind ("M-0" . treemacs-select-window))

(use-package! window
  :defer t
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

(use-package! avy
  :defer t
  :bind (("M-g g" . avy-goto-line)
         ("M-g M-g" . avy-goto-line)
         ("C-:" . avy-goto-char)))

(use-package! goto-addr
  :defer t
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

(use-package! multiple-cursors
  :defer t
  :hook
  (prog-mode . multiple-cursors-mode)
  :bind (("C-c m" . mc/edit-lines)))

(use-package! whole-line-or-region
  :init (whole-line-or-region-global-mode))

;; ================= PROJECT =================

(use-package! projectile
  :defer t
  :config (setq projectile-project-search-path '(("~/Work/" . 3))
                projectile-switch-project-action #'projectile-dired))

;; ================= COMPLETION =================

(use-package! company
  :defer t
  :config (setq company-idle-delay 0.3
                company-minimum-prefix-length 2
                company-require-match nil)
  :bind (:map company-mode-map
          ("C-." . company-complete)))

(use-package! company-box
  :defer t
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
  :defer t
  :after (company yasnippet)
  :bind ("M-/" . company-yasnippet))

(use-package! consult
  :defer t
  :bind (("C-s" . consult-line)
         ("C-r" . consult-line)))

;; =================== VCS ===================

(use-package! magit
  :defer t
  :bind
  ("C-c v m" . magit-checkout)
  ("C-c v e" . magit-ediff-resolve)
  ("C-c v P" . magit-push))

;; ================= CHECKER =================

(use-package! flycheck
  :defer t
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
  :defer t
  :init
  ;; FIXME: Somwehow ccls' lens show up in other modes.
  (remove-hook 'lsp-lens-mode-hook 'ccls-code-lens-mode)
  (add-hook 'lsp-mode-hook 'lsp-lens-mode)
  :config (setq lsp-lens-enable t))

(use-package! lsp-ui
  :defer t
  :config (setq lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-show-with-mouse t))

;; ================= DEV =================

;; Alloy.
(use-package! alloy-mode
  :defer t
  :mode "\\.als\\'")


;; C/C++.
(use-package! cpp-auto-include
  :defer t
  :bind (:map c++-mode-map
          ("C-c c o" . cpp-auto-include)))

(use-package! cmake-mode
  :defer t
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

(use-package! cmake-ide
  :defer t
  :after (cmake-mode projectile)
  :init (cmake-ide-setup)
  :hook ((c-mode c++-mode) . my/cmake-ide-find-project)
  :preface
  (defun my/cmake-ide-find-project ()
    "Finds the directory of the project for cmake-ide."
    (if (not (null (projectile-project-root)))
      (with-eval-after-load 'projectile
        (setq cmake-ide-project-dir (projectile-project-root))
              cmake-ide-build-dir (concat cmake-ide-project-dir "build")
              cmake-ide-compile-command
                (concat "cmake -B " cmake-ide-build-dir " && cmake --build " cmake-ide-build-dir))
      (cmake-ide-load-db)))

  (defun my/switch-to-compilation-window ()
    "Switches to the *compilation* buffer after compilation."
    (other-window 1))
  :bind ([remap comment-region] . cmake-ide-compile)
    (:map cmake-mode-map
      ("C-c C-c C-c" . cmake-ide-compile))
  :config (advice-add 'cmake-ide-compile :after #'my/switch-to-compilation-window))

(use-package! flycheck-clang-tidy
  :defer t
  :after flycheck
  :config
  (setq flycheck-clang-tidy-extra-options
        ;; Available checks: https://clang.llvm.org/extra/clang-tidy/.
        (concat "--checks=-*,clang-analyzer-*,cppcoreguidelines-*,google-*"))
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup)
  ((c-mode c++-mode) . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (c/c++-clang-tidy)))))))))

(use-package! google-c-style
  :defer t
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))


;; Docker
(use-package! dockerfile-mode
  :defer t
  :hook
  (dockerfile-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (dockerfile-hadolint)))))))))


;; Go.
(use-package! flycheck-golangci-lint
  :defer t
  :after flycheck
  :config
  ;; Enable this if things slow down significantly.
  ;; (setq flycheck-golangci-lint-fast t)
  :hook
  (flycheck-mode . flycheck-golangci-lint-setup)
  (go-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))

(use-package! dap-go
  :defer t
  :after (dap-mode go-mode)
  :config (dap-go-setup))


;; Graphviz.
(use-package! graphviz-dot-mode
  :defer t
  :config (setq graphviz-dot-indent-width 4))

(use-package! company-graphviz-dot
  :defer t
  :after graphviz-dot-mode)

;; Haskell.
(use-package! haskell-cabal
  :defer t
  :mode ("\\.cabal\\'" . haskell-cabal-mode)
  :bind (:map haskell-cabal-mode-map
          ("C-c C-c C-c" . haskell-process-cabal-build)))


;; Ini.
(use-package! ini-mode
  :defer t
  :mode "\\.ini\\'")


;; Idris.
(use-package! idris-mode
  :defer t
  :custom
  (idris-interpreter-path "idris2"))


;; Java.
(use-package! gradle-mode
  :defer t
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
  :defer t
  :commands (kubernetes-overview)
  :config (setq kubernetes-poll-frequency 3600
                kubernetes-redraw-frequency 3600))


;; Lean
(use-package! lean4-mode
  :defer t
  :mode "\\.lean\\'")


;; Markdown.
(use-package! markdown-mode
  :defer t
  :mode ("\\.\\(md\\|markdown\\)\\'")
  :hook
    (markdown-mode . lsp-deferred)
    (markdown-mode . pandoc-mode)
  :custom (markdown-command "pandoc"))

(use-package! markdown-preview-mode
  :defer t
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
  :defer t
  :bind (:map markdown-mode-map
              ("C-c C-c C-c" . pandoc-run-pandoc)))


;; Nix.
(use-package! nix-mode
  :defer t
  :hook (nix-mode . lsp-deferred))


;; Protobuf.
(use-package! protobuf-mode
  :defer t)


;; Python.
(use-package! lsp-pyright
  :defer t
  :config (setq lsp-pyright-typechecking-mode "basic")
  :hook
  (python-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint)))))))))


;; Rust.
(use-package! rustic
  :defer t
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-server-display-inlay-hints t)
  :bind (:map rustic-mode-map
          ("C-c C-c C-t" . rustic-cargo-test-rerun)
          ("C-c C-c C-d" . rustic-cargo-doc)
          ("C-c C-c C-a" . rustic-cargo-add)
          ("C-c C-c C-e" . lsp-rust-analyzer-expand-macro)))

(use-package! dap-cpptools
  :defer t
  :after (dap-mode rustic)
  :config (dap-cpptools-setup))

;; Scala.
(use-package! lsp-metals
  :defer t
  :config (setq lsp-metals-show-inferred-type t))


;; Shell.
(use-package! sh-script
  :defer t
  :hook
  (sh-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (sh-shellcheck)))))))))


;; Sql.
(use-package! sql
  :defer t
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
  :defer t
  :after sql
  :config (setq sqlformat-command 'sqlfluff)
  :bind (:map sql-mode-map
          ("C-c c f" . 'sqlformat)))

(use-package! sqlup-mode
  :defer t
  :after sql
  :init
  (add-hook 'sql-mode-hook 'sqlup-mode)
  :bind (:map sql-mode-map
          ("C-c c u r" . sqlup-capitalize-keyworks-in-region)
          ("C-c c u b" . sqlup-capitalize-keyworks-in-buffer)))


;; Systemd.
(use-package! systemd
  :defer t)


;; Yang.
(use-package! yang-mode
  :defer t)


;; Xml.
(use-package! nxml-mode
  :defer t
  :hook (nxml-mode . lsp-deferred)
  :config (setq nxml-child-indent 2
        nxml-attribute-indent 4
        nxml-slash-auto-complete-flag t))