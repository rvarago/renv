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

(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (magit-todos-mode 1)
  :bind (:map magit-mode-map
              ("C-t" . magit-todos-list)))


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

;; Misc.
(defun my/find-file-in-project (filenames)
  "Return the project directory containing one of the FILENAMES or nil if none are found."
  (let* ((project-root (projectile-project-root))
         (current-dir default-directory)
         (file-found nil))
    (catch 'found
      ;; 1. Check if any of the filenames exist in the project root.
      (dolist (filename filenames)
        (if (file-exists-p (expand-file-name filename project-root))
            (throw 'found project-root)))

      ;; 2. Traverse upwards from the current directory to the project root.
      (let ((dir current-dir))
        (while (and dir (not file-found))
          (dolist (filename filenames)
            (if (file-exists-p (expand-file-name filename dir))
                (progn
                  (setq file-found t)
                  (throw 'found dir))))
          ;; Move up one directory level
          (setq dir (file-name-directory (directory-file-name dir)))))

      ;; 3. If a file was found, return the corresponding directory.
      (if file-found
          (expand-file-name (car filenames) current-dir)
        nil))))

;; DAP.
(map! :map dap-mode-map
      :prefix "C-c C-d"
      "t" #'dap-debug-edit-template
      "d" #'dap-debug
      "l" #'dap-debug-last
      "b a" #'dap-breakpoint-add
      "b d" #'dap-breakpoint-delete
      "b k" #'dap-breakpoint-delete-all
      "c" #'dap-continue
      "n" #'dap-next
      "i" #'dap-step-in
      "o" #'dap-step-out
      "e e" #'dap-eval
      "e r" #'dap-eval-region
      "e t" #'dap-eval-thing-at-point
      "s d" #'dap-delete-session
      "s k" #'dap-delete-all-sessions)


;; Alloy.
(use-package! alloy-mode
  :defer t
  :mode "\\.als\\'")

(defvar my/cmake-project-files '("CMakeLists.txt"))

;; C/C++.
(defun my/cmake-configure (&optional suffix)
  "Configure the CMake project. Optionally use a suffix for the build directory (e.g., 'linux'). All env-vars with RV_LOCAL_CMAKE_ are forwarded as -D with the prefix removed."
  (interactive "sEnter build suffix (e.g., linux, or leave blank): ")
  (let* ((project-root (my/find-file-in-project my/cmake-project-files))
         (build-name (if (string-empty-p suffix)
                         "build"
                       (concat "build_" suffix)))
         (build-dir (concat project-root build-name))
         (source-dir project-root)
         (env-var-prefix "RV_LOCAL_CMAKE_")
         (env-vars (cl-remove-if-not (lambda (var) (string-prefix-p env-var-prefix var))
                                     process-environment))
         (env-vars-keys (mapcar (lambda (env) (car (split-string env "="))) env-vars))
         (extra-args (concat "-DCMAKE_EXPORT_COMPILE_COMMANDS=YES" " " (mapconcat
                                                                        (lambda (var)
                                                                          (let* ((env-value (getenv var))
                                                                                 (cmake-var (string-remove-prefix env-var-prefix var)))
                                                                            (concat "-D" cmake-var "=" env-value)))
                                                                        env-vars-keys
                                                                        " "))))
    (if (file-exists-p (concat project-root "CMakeLists.txt"))
        (progn
          (make-directory (concat project-root build-dir) t)
          (message "Configuring CMake in %s..." build-dir)
          (compile (concat "cmake" " -B" build-dir " -S" source-dir " " extra-args)))
      (message "No CMakeLists.txt found in the project root."))))


(defun my/cmake-build (&optional suffix target)
  "Build the CMake project. Accepts an optional SUFFIX for the build directory and an optional TARGET."
  (interactive "sEnter build suffix (e.g., linux, or leave blank): \nsEnter target name (Leave blank for default target): ")
  (let* ((project-root (my/find-file-in-project my/cmake-project-files))
         (build-name (if (string-empty-p suffix)
                         "build"
                       (concat "build_" suffix)))
         (build-dir (concat project-root build-name)))
    (if (file-exists-p (concat project-root "CMakeLists.txt"))
        (let ((build-command (if (string-empty-p target)
                                 (concat "cmake --build " build-dir)
                               (concat "cmake --build " build-dir " --target " target))))
          (message "Building CMake project in %s..." build-dir)
          (compile build-command))
      (message "No CMakeLists.txt found in the project root."))))

(defun my/cmake-test-run (&optional suffix test-name)
  "Run ctest with an optional SUFFIX appended to the build name.
If TEST-NAME is provided, use it. Otherwise, if a region is selected,
use the region as the test name. If no TEST-NAME or region is provided,
run all tests."
  (interactive "sEnter build suffix (e.g., linux): \nsEnter optional test name (leave empty for all tests): ")
  (let* ((project-root (my/find-file-in-project my/cmake-project-files))
         (build-name (if (string-empty-p suffix)
                         "build"
                       (concat "build_" suffix)))
         (build-dir (concat project-root build-name))
         (test-to-run (cond
                       ((not (string-empty-p test-name)) test-name)
                       ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
                       (t nil)))
         (test-filter (if (null test-to-run)
                          "'.*'"
                        (concat "\"" test-to-run "\"")))
         (command (concat "ctest --test-dir " build-dir " -R " test-filter)))

    (if (file-exists-p (concat project-root "CMakeLists.txt"))
        (progn
          (message "Running ctest...")
          (compile command))
      (message "No CMakeLists.txt found in the project root."))))

(map! :map c-mode-base-map
      :prefix "C-c C-c"
      "C-c" #'my/cmake-configure
      "C-b" #'my/cmake-build
      "C-t" #'my/cmake-test-run)

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


;; Copilot.
(use-package! copilot
  :config
  ;; Disable automatic completions
  (setq copilot-idle-delay nil) ;; ensures no auto-trigger
  (setq copilot-disable-predicates '(t))
  (setq copilot-indent-offset-warning-disable t)
  :hook (prog-mode . copilot-mode)
  :bind (
         :map prog-mode-map
              ("C-c C-." . 'copilot-complete)
         :map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(use-package! copilot-chat
  :after copilot
  :config
  ;; Set Claude Sonnet 4.5 as the model
  ;; (setq copilot-chat-backend 'anthropic)
  ;; (setq copilot-chat-anthropic-model "claude-sonnet-4-5-20250929")

  ;; Optional: Set Anthropic API key (or use environment variable ANTHROPIC_API_KEY)
  ;; (setq copilot-chat-anthropic-api-key "your-api-key-here")

  (map! (:prefix ("C-c C-a")
         :desc "Copilot Chat" "c" #'copilot-chat-display
         :desc "Copilot Chat Explain" "e" #'copilot-chat-explain
         :desc "Copilot Chat Review" "r" #'copilot-chat-review
         :desc "Copilot Chat Doc" "d" #'copilot-chat-doc
         :desc "Copilot Chat Fix" "f" #'copilot-chat-fix
         :desc "Copilot Chat Optimize" "o" #'copilot-chat-optimize
         :desc "Copilot Chat Test" "t" #'copilot-chat-test
         :desc "Copilot Chat Custom Prompt" "p" #'copilot-chat-custom-prompt-selection))

  ;; Configure chat window behavior
  (setq copilot-chat-window-config
        '((display-buffer-in-side-window)
          (side . right)
          (window-width . 0.5))))


;; Docker
(use-package! dockerfile-mode
  :defer t
  :hook
  (dockerfile-mode . (lambda () (setq flycheck-local-checkers '((lsp . ((next-checkers . (dockerfile-hadolint)))))))))


;; Env.
(after! envrc-mode
  (map!
   :prefix "C-c"
   :map envrc-mode-map
   "C-a" #'envrc-allow
   "C-l" #'envrc-reload))

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


;; Swift
(after! swift-mode
  (map!
   :prefix "C-c C-c"
   :map swift-mode-map
   "C-r" #'swift-mode:run-repl
   "C-s" #'swift-mode:send-region
   "C-b" #'swift-mode:send-buffer))


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
