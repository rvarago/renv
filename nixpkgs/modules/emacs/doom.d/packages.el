;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Completion.
(package! company-box)

;; Alloy.
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode"))

;; C/C++.
(package! cpp-auto-include)
(package! cmake-ide)
(package! flycheck-clang-tidy)
(package! google-c-style)

;; Edit.
(package! move-text)
(package! multiple-cursors)
(package! whole-line-or-region)

;; Graphviz.
(package! graphviz-dot-mode)

;; Ini.
(package! ini-mode)

;; Kubernetes.
(package! kubernetes)

;; Lean
(package! lean4-mode
    :recipe (:host github :repo "leanprover/lean4-mode"))

;; Markdown.
(package! markdown-preview-mode)
(package! pandoc-mode)

;; Protobuf.
(package! protobuf-mode)

;; Sql.
(package! sql)
(package! sqlformat)
(package! sqlup-mode)

;; Systemd.
(package! systemd)

;; UI.
(package! auto-dim-other-buffers)

;; Yang.
(package! yang-mode)
