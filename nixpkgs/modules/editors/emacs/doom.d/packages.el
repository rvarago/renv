;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Alloy.
(package! alloy-mode
    :recipe (:host github :repo "dwwmmn/alloy-mode") :pin "0d05bdd10c77ec04c3d61eccf67e68c08284951f")

;; C/C++.
(package! cpp-auto-include :pin "0ce829f27d466c083e78b9fe210dcfa61fb417f4")
(package! cmake-ide :pin "28dc4ab5bd01d99553901b4efeb7234280928b18")
(package! flycheck-clang-tidy :pin "f9ae7306bd6ca08b689b36c1e8f6f6b91d61db5f")
(package! google-c-style :pin "d2cc2a7ec3339b83a9fdae5f9eb2bc6f732aa43c")

;; Edit.
(package! move-text :pin "cbcd0e5a316f26a8e8ef67ea0e633a43b045aa1e")
(package! multiple-cursors :pin "6956e8e12ee191d7c80d042ae8ff495286fcbe38")
(package! whole-line-or-region :pin "f7c9b25f8c932cff8239cde4a76e48135bdfa7e6")

;; Graphviz.
(package! graphviz-dot-mode :pin "8ff793b13707cb511875f56e167ff7f980a31136")

;; Ini.
(package! ini-mode :pin "5472abc94e564edc6b469c48d2324519a044a77c")

;; Java.
(package! gradle-mode :pin "e4d665d5784ecda7ddfba015f07c69be3cfc45f2")

;; Kubernetes.
(package! kubernetes :pin "b0fb60564e818c30d6fc8744e229c8ed0707ce7a")

;; Lean
(package! lean4-mode
    :recipe (:host github :repo "leanprover/lean4-mode") :pin "2c6ef33f476fdf5eb5e4fa4fa023ba8b11372440")

;; Markdown.
(package! markdown-preview-mode :pin "85fbfec507a222326695a3b91ff7306d0c4f94c6")
(package! pandoc-mode :pin "8f955abec9c1d75acd9b03389b90a276ec4e2137")

;; PDF.
(package! pdf-tools :built-in 'prefer :pin "7ff6293a25baaae65651b3e1c54b61208279a7ef")

;; Protobuf.
(package! protobuf-mode :pin "f11079b01f075bc7c5b3286d85a8feca798c09e9")

;; Sql.
(package! sql)
(package! sqlformat :pin "3fa86085de8a4e70954d4b3346fb228016b5bbb9")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")

;; Systemd.
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")

;; UI.
(package! auto-dim-other-buffers :pin "33b5f88b799a17947c266b04ad59462c5aeb4ed7")

;; Yang.
(package! yang-mode :pin "4b4ab4d4a79d37d6c31c6ea7cccbc425e0b1eded")
