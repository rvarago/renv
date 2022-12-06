;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Alloy.
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode") :pin "0d05bdd10c77ec04c3d61eccf67e68c08284951f")

;; C/C++.
(package! cpp-auto-include :pin "0ce829f27d466c083e78b9fe210dcfa61fb417f4")
(package! cmake-ide :pin "28dc4ab5bd01d99553901b4efeb7234280928b18")
(package! flycheck-clang-tidy :pin "f9ae7306bd6ca08b689b36c1e8f6f6b91d61db5f")
(package! google-c-style :pin "307bda3459cc785c1ffe1db385aad8bed5f7bc20")

;; Edit.
(package! move-text :pin "cbcd0e5a316f26a8e8ef67ea0e633a43b045aa1e")
(package! multiple-cursors :pin "fe0d5167459b792a699af782685582a195852cb9")
(package! whole-line-or-region :pin "f7c9b25f8c932cff8239cde4a76e48135bdfa7e6")

;; Graphviz.
(package! graphviz-dot-mode :pin "46afe4825b4a3a5d1a1ac65ec7f585051f756b78")

;; Ini.
(package! ini-mode :pin "2194cfa2fd13196a37350ec20b3f00dcf6162b7c")

;; Java.
(package! gradle-mode :pin "e4d665d5784ecda7ddfba015f07c69be3cfc45f2")

;; Kubernetes.
(package! kubernetes :pin "8163fd38015cbde0485f6eaab41450132bf6e19d")

;; Lean
(package! lean4-mode
    :recipe (:host github :repo "leanprover/lean4-mode") :pin "37d5c99b7b29c80ab78321edd6773200deb0bca6")

;; Markdown.
(package! markdown-preview-mode :pin "85fbfec507a222326695a3b91ff7306d0c4f94c6")
(package! pandoc-mode :pin "8f955abec9c1d75acd9b03389b90a276ec4e2137")

;; PDF.
(package! pdf-tools :built-in 'prefer :pin "b8079e4ebc2936f9772657332d50936350a65825")

;; Protobuf.
(package! protobuf-mode :pin "72b9420459435c3b5a2c4fd0a52824bc346051da")

;; Sql.
(package! sql)
(package! sqlformat :pin "3fa86085de8a4e70954d4b3346fb228016b5bbb9")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")

;; Systemd.
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

;; UI.
(package! auto-dim-other-buffers :pin "33b5f88b799a17947c266b04ad59462c5aeb4ed7")

;; Yang.
(package! yang-mode :pin "4b4ab4d4a79d37d6c31c6ea7cccbc425e0b1eded")
