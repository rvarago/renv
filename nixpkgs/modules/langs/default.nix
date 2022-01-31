{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    # Agda.
    (agda.withPackages (p: [ p.standard-library ]))

    # Alloy.
    alloy6

    # C/C++.
    ccls
    clang-tools
    cmake
    conan
    cmake-language-server

    # Clojure.
    clojure
    leiningen

    # Coq.
    coq

    # Common-Lisp
    sbcl
    lispPackages.quicklisp

    # Elm.
    elmPackages.elm
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm-test

    # Go.
    delve
    go
    golangci-lint
    gopls
    gopkgs
    gore
    gotests
    gotools
    go-outline

    # Haskell.
    ghc
    haskellPackages.cabal2nix
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.retrie
    haskellPackages.stack

    # Idris.
    idris2

    # Java.
    gradle
    openjdk
    maven

    # Lean4.
    elan
    
    # JS/TS.
    nodejs
    nodePackages.eslint
    nodePackages.prettier
    nodePackages.typescript
    yarn

    # .Net.
    dotnetCorePackages.sdk_5_0

    # Nix.
    nixpkgs-fmt
    rnix-lsp

    # Python.
    python3
    python3Packages.autopep8
    python3Packages.black
    python3Packages.ipython
    python3Packages.isort
    python3Packages.pip
    python3Packages.poetry
    python3Packages.pyflakes
    python3Packages.pylint
    python3Packages.pytest
    python3Packages.setuptools

    # OCaml.
    ocamlformat
    ocamlPackages.ocaml-lsp
    ocamlPackages.utop
    opam

    # Rust.
    cargo-audit
    cargo-edit
    cargo-expand
    cargo-license
    cargo-modules
    cargo-outdated
    cargo-tarpaulin
    cargo-udeps
    cargo-watch
    rustup
    rust-analyzer
    sqlx-cli

    # Scala.
    ammonite
    sbt
    scala

    # Scheme.
    racket
    guile

    # Shell.
    shellcheck
    shfmt

    # SQL.
    sqls
    sqlfluff
    sqlint

    # Text
    nodePackages.unified-language-server

    # TLA+
    tlaps
    tlaplus
    # tla-toolbox

    # Web
    html-tidy
  ];

  home.file.".ghc/ghci.conf".text = ''
    :set prompt "λ> "
    :set prompt-cont "λ| "

    :seti -XDataKinds
    :seti -XTypeApplications
    :seti -XTypeOperators
  '';
  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };
}
