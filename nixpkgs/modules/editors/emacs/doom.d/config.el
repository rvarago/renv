;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; ================= USER =================

(setq user-full-name (getenv "USER_FULL_NAME")
      user-mail-address (getenv "USER_EMAIL"))

;; ================= UI =================

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(setq doom-theme 'doom-zenburn
      doom-themes-treemacs-theme "doom-colors"
      doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 30)
      doom-themes-treemacs-enable-variable-pitch nil
      display-line-numbers-type t
      doom-scratch-initial-major-mode 'lisp-interaction-mode)

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
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)))

(use-package! treemacs
  :defer t
  :after projectile
  :init (add-hook 'projectile-after-switch-project-hook #'treemacs-add-and-display-current-project-exclusively)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t))

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

(map! "C-^" #'doom/increase-font-size)
(map! "C-%" #'doom/decrease-font-size)
(map! "C-&" #'doom/reset-font-size)

;; ================= EDIT =================

(map! "<mouse-8>" #'xref-pop-marker-stack)

(map! "C-c d" #'delete-region)

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(setq org-directory "~/org/")

(defun my/mark-word ()
  "Mark a region around the word beginning."
  (let ((begin (save-excursion
                 (backward-word)
                 (point))))
    (forward-word)
    (set-mark begin)))

(defun my/wrap-region-or-words-pairs (wrapper)
  (pcase wrapper
    ("{"  '("{" "}"))
    ("}"  '("{" "}"))
    ("("  '("(" ")"))
    (")"  '("(" ")"))
    ("["  '("[" "]"))
    ("]"  '("[" "]"))
    ("<"  '("<" ">"))
    (">"  '("<" ">"))
    (_ (list wrapper wrapper))))


(defun my/wrap-region-or-word ()
  "Wrap a region (if selected) or a word (otherwise) between user-provided wrappers."
  (interactive)
  (unless (region-active-p)
    (my/mark-word))
  (let* ((user-wrapper (read-string "Wrapper (open or close): "))
         (content (buffer-substring (region-beginning) (region-end)))
         (wrappers (my/wrap-region-or-words-pairs user-wrapper))
         (wrappers-open (car wrappers))
         (wrappers-close (car (cdr wrappers))))
    (delete-active-region)
    (insert wrappers-open content wrappers-close)))


(map! "C-x w" #'my/wrap-region-or-word)

(use-package! avy
  :defer t
  :bind (("M-g M-g" . avy-goto-line)
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

(use-package! rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(map! "C-c C-s" #'replace-string)
(map! "C-c C-r" #'query-replace-regexp)

(add-hook! prog-mode-hook #'subword-mode)

(defun my/blog-new-entry ()
  "Prompt for a title and create an org file read to be blogged."
  (interactive)
  (let* ((title (read-string "Title: "))
         (current-date (format-time-string "%Y-%m-%d"))
         (sanitized-title (downcase (replace-regexp-in-string "[[:space:][:punct:]]+" "-" title)))
         (file-name (concat current-date "-" sanitized-title ".org"))
         (file-content (concat "#+begin_export html\n"
                               "---\n"
                               "layout: post\n"
                               (format "title: %s\n" title)
                               "permalink: /:title/\n"
                               "tags: []\n"
                               "---\n"
                               "#+end_export\n\n"
                               "#+begin_quote\n"
                               "/abstract/\n"
                               "#+end_quote\n"
                               "--------------\n\n"))
         (project-root (projectile-project-root))
         (posts-dir (expand-file-name "_posts" project-root)))

    (unless (file-directory-p posts-dir)
      (make-directory posts-dir t))

    (let ((full-file-path (expand-file-name file-name posts-dir)))

      (with-temp-file full-file-path
        (insert file-content))

      (find-file full-file-path)
      (message "Have fun blogging at: %s" full-file-path))))

;; ================= PROJECT =================

(after! projectile
  (setq projectile-project-search-path '(("~/Work/" . 3))))

;; (defun my/workspace-switch-action (dir)
;;   "Switch to the project documentation if there's any or to dired otherwise"
;;   (let* ((files '("README.org" "README.md" "README"))
;;          (file-matching (seq-find #'file-exists-p files)))
;;     (if file-matching
;;         (find-file (concat (file-name-as-directory dir) file-matching))
;;       (projectile-dired))))
;;
;; (setq +workspaces-switch-project-function #'my/workspace-switch-action)

;; ================= COMPLETION =================

(use-package! company
  :defer t
  :config
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1)
  :bind (("C-." . company-complete)))

(after! company-yasnippet
  (map! :map company-mode-map "M-/" #'company-yasnippet))

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

(use-package! git-link
  :defer t
  :custom
  (git-link-use-commit t)
  (git-link-use-single-line-number t)
  :commands (git-link git-link-commit git-link-homepage)
  :bind
  ("C-c v &" . git-link))

;; ================= SHELL =================

(after! esh-mode
  (set-eshell-alias!
   "nr" "nix-shell -p $*"
   "nrepl" "nix repl $*"
   "nshell" "nix shell $*"
   "ndev" "nix develop $*"
   "nbuild" "nix build $*"
   "nrun" "nix run $*"

   "g" "git --no-pager $*")
  (map! :map eshell-mode-map
        :prefix "C-c"
        "\\" #'+eshell/split-right
        "-" #'+eshell/split-below))

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
  :bind
  ("C-c x" . my/flycheck-switch-to-list-errors))

;; ================= LSP =================

(use-package! lsp-mode
  :defer t
  :init
  (add-hook 'lsp-after-open-hook 'lsp-lens-mode)
  :config (setq lsp-lens-enable t
                lsp-auto-execute-action nil
                lsp-verify-signature t))

(use-package! lsp-ui
  :defer t
  :config (setq lsp-ui-doc-show-with-cursor nil
                lsp-ui-doc-show-with-mouse t))

;; ================= DEV =================

;; General.
;; (global-set-key (kbd "C-<") 'xref-go-back)
;; (global-set-key (kbd "C->") 'xref-go-forward)

;; Alloy.
(use-package! alloy-mode
  :defer t
  :mode "\\.als\\'")


;; C/C++.
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
        (concat "--checks=-*,clang-analyzer-*,cppcoreguidelines-*,google-*,-readability-identifier-length,-readability-magic-numbers,-cppcoreguidelines-avoid-magic-numbers"))
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
(after! go-mode
  (map!
   :prefix "C-c C-c"
   :map go-mode-map
   "C-a" #'+go/test-all
   "C-t" #'+go/test-single
   "C-r" #'+go/test-rerun
   "t f" #'+go/test-file
   "t n" #'+go/test-nested
   "C-b" #'+go/bench-single
   "C-i" #'go-goto-imports
   "C-s" #'gorepl-eval-region
   "C-l" #'gorepl-eval-line
   :map gorepl-mode-map
   "C-a" #'gorepl-import))(after! go-mode
  (map! :map go-mode-map :prefix "C-c C-c"
        "C-a" #'+go/test-all
        "C-t" #'+go/test-single
        "C-r" #'+go/test-rerun
        "t f" #'+go/test-file
        "t n" #'+go/test-nested
        "C-b" #'+go/bench-single
        "C-i" #'go-goto-imports
        "C-s" #'gorepl-eval-region
        "C-l" #'gorepl-eval-line)
  (map!   :map gorepl-mode-map
          "C-a" #'gorepl-import))


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
  :defer t)


;; Java.
(use-package! lsp-java
  :defer t
  :config (setq lsp-java-references-code-lens-enabled t
                lsp-java-implementations-code-lens-enabled t)
  :bind (:map java-mode-map
              ("C-c C-c C-b" . lsp-jt-browser)
              ("C-c C-c C-r" . dap-java-debug)))

(use-package! gradle-mode
  :defer t)


;; Kubernetes.
(use-package! kubernetes
  :defer t
  :commands (kubernetes-overview)
  :config (setq kubernetes-poll-frequency 3600
                kubernetes-redraw-frequency 3600))


;; Lean
(use-package! lean4-mode
  :defer t)


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
  :bind (:map text-mode-map
              ("C-c C-c C-c" . pandoc-run-pandoc)))


;; Nix.
(use-package! nix-mode
  :defer t
  :hook (nix-mode . lsp-deferred))


;; Protobuf.
(use-package! protobuf-mode
  :defer t)


;; Python.
(after! python
  (setq lsp-pyright-type-checking-mode "strict")
  (add-hook 'python-mode-hook
            (lambda ()
              (setq flycheck-local-checkers '((lsp . ((next-checkers . (python-pylint))))))))
  (map!
   :map python-mode-map
   :prefix "C-c C-c"
   "C-a" #'poetry-add
   "C-o" #'lsp-pyright-organize-imports
   "C-s" #'python-shell-send-buffer
   "s f" #'python-shell-send-file
   "s r" #'python-shell-send-region
   "s d" #'python-shell-send-defun
   "s s" #'python-shell-send-string
   "C-t" #'python-pytest-run-def-or-class-at-point
   "C-r" #'python-pytest-repeat
   "t f" #'python-pytest-file))

;; Ruby.
(use-package! ruby-mode
  :defer t
  :config (setq lsp-solargraph-use-bundler t))


;; Rust.
(use-package! rustic
  :defer t
  :init
  (require 'dap-cpptools)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "check"
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-chaining-hints t)
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
  :config (setq lsp-metals-show-inferred-type t
                lsp-metals-show-implicit-arguments nil
                lsp-metals-show-implicit-conversions-and-classes nil
                lsp-metals-super-method-lenses-enabled t))

(map! :map scala-mode-map
      "C-c C-c C-s"  #'sbt-start
      "C-c C-c C-b"  #'sbt-switch-to-active-sbt-buffer
      "C-c C-c C-k"  #'sbt-do-clean
      "C-c C-c C-c"  #'sbt-do-compile
      "C-c C-c C-t"  #'sbt-do-test
      "C-c C-c C-r"  #'sbt-do-run
      "C-c C-c C-o"  #'sbt-command
      "C-c C-c C-p"  #'sbt-run-previous-command)


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
