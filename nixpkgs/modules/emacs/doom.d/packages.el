;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; Alloy.
;;(package! alloy-mode
;;    :recipe (:host github :repo "dwwmmn/alloy-mode") :pin "0d05bdd10c77ec04c3d61eccf67e68c08284951f")

;; Bitbake.
(package! bitbake-el
  :recipe (:host github :repo "canatella/bitbake-el") :pin "44513a330d3bb2bceb1bfd99b4eb63b37f681369")

;; C/C++.
(package! flycheck-clang-tidy :pin "f9ae7306bd6ca08b689b36c1e8f6f6b91d61db5f")
(package! google-c-style :pin "b5c8ecae344b5697210b9aa9a3ce08b608cea239")

;; AI.
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")) :pin "ab5c58bc969f52f6d75e972658f2c3381c70b4fa")

(package! copilot-chat :recipe (:host github :repo "chep/copilot-chat.el") :pin "444c7a5114ba483c3c37fb95ea2888c5539fc9c8")

(package! shell-maker
  :recipe (:host github :repo "xenodium/shell-maker" :files ("*.el")) :pin "6377cbdb49248d670170f1c8dbe045648063583e")

(package! chatgpt-shell
  :recipe (:host github :repo "xenodium/chatgpt-shell" :files ("*.el")) :pin "cbad6ffc9cbde1962a7317cf6d1d4d50ac1c1ac2")

(package! acp
  :recipe (:host github :repo "xenodium/acp.el" :files ("*.el")) :pin "c32fbf8df34ed0095853a8cf55dc783e68b67d90")

(package! agent-shell
  :recipe (:host github :repo "xenodium/agent-shell" :files ("*.el")) :pin "023ad9caaf9b66162d58c6189e61622ee95d13c8")

(package! gptel-agent
  :recipe (:host github :repo "karthink/gptel-agent" :files ("*.el")) :pin "e2ef97d6b566b2ad751c8a0a87b8272710c95808")

;; Go.
(package! go-fill-struct
  :recipe (:host github :repo "s-kostyaev/go-fill-struct") :pin "9e2e4be5af716ecadba809e73ddc95d4c772b2d9")

(package! go-fill-struct
  :recipe (:repo "emacsorphanage/go-impl") :pin "1eebba6ccd02d11a5a82ad4540a8d562797bc3b3")

(package! go-playground
  :recipe (:repo "grafov/go-playground") :pin "5726251414d3d7cc05fd54566ee9149808501574")

;; Edit.
(package! move-text :pin "2a8ebefeb0b363681e9562847eca3fd66e090d70")
(package! multiple-cursors :pin "94b8b07a4bab87f803123723b68227565429dfa1")
(package! mmm-mode :pin "b1f5c7dbdc405e6e10d9ddd99a43a6b2ad61b176")
(package! whole-line-or-region :pin "e854a446d36bf0af54b13730ceaaf0c75e636662")

;; VCS.
(package! magit-todos :pin "7294a95580bddf7232f2d205efae312dc24c5f61")
(package! git-link :pin "b651de43236276cdb18ec7727f645cbf6743a499")

;; Graphviz.
(package! graphviz-dot-mode :pin "516c151b845a3eb2da73eb4ee648ad99172087ac")

;; Idris.
(package! idris-mode
  :recipe (:host github :repo "idris-hackers/idris-mode") :pin "d32b2396a8ad17820e308cd267f1b464a5235abc")

;; Ini.
(package! ini-mode :pin "d99a27548a650b8ad531634419ae55f7b4dbe2fa")

;; Java.
(package! gradle-mode :pin "e4d665d5784ecda7ddfba015f07c69be3cfc45f2")

;; Kubernetes.
(package! kubernetes :pin "54ad1b11ca6ceff8a7931271d8c694ad2e6ade4c")

;; Lean
(package! lean4-mode
  :recipe (:host github :repo "leanprover/lean4-mode" :files ("*.el" "data")) :pin "1388f9d1429e38a39ab913c6daae55f6ce799479")

;; Markdown.
(package! markdown-preview-mode :pin "68242b3907dc065aa35412bfd928b43d8052d321")
(package! pandoc-mode :pin "8f46da90228a9ce22de24da234ba53860257640a")

;; PDF.
(package! pdf-tools :built-in 'prefer :pin "365f88238f46f9b1425685562105881800f10386")

;; Protobuf.
(package! protobuf-mode :pin "09707c5d95a41451abd65c6a21a9694070359cdd")

;; Sql.
(package! sql)
(package! sqlformat :pin "69ef8bec3ac8c9ce4a60f20a9721e4d7a592a84d")
(package! sqlup-mode :pin "3f9df9c88d6a7f9b1ae907e401cad8d3d7d63bbf")

;; Systemd.
(package! systemd :pin "8742607120fbc440821acbc351fda1e8e68a8806")

;; UI.
(package! auto-dim-other-buffers :pin "d8591d048f97478e75c71830fb6d7c009351c73d")

;; Yang.
(package! yang-mode :pin "b7a4c1734a60f70d80d5752ae058232df0b18336")
