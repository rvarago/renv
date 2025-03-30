{ pkgs, ... }:

{

  home.sessionVariables = {
    JAVA_HOME = "${pkgs.openjdk}/lib/openjdk";
  };

  home.packages = with pkgs; [
    # # Agda.
    # (agda.withPackages (p: [ p.standard-library ]))

    # # Alloy.
    # alloy6

    # # C/C++.
    # ccls
    # clang-tools
    # cmake
    # conan
    # cmake-language-server

    # Clojure.
    # clojure
    # leiningen

    # # Coq.
    # coq

    # # Common-Lisp.
    # sbcl
    # lispPackages.quicklisp

    # Dhall
    dhall
    dhall-json

    # # Elm.
    # elmPackages.elm
    # elmPackages.elm-analyse
    # elmPackages.elm-format
    # elmPackages.elm-test

    # Go.
    delve
    go
    # golangci-lint
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

    # # Idris.
    # idris2

    # Java.
    gradle
    openjdk
    maven

    # JS/TS.
    nodejs
    # nodePackages.eslint
    # nodePackages.prettier
    nodePackages.typescript
    yarn

    # # Kotlin.
    # kotlin

    # # Lean4.
    # elan

    # # .Net.
    # dotnetCorePackages.sdk_6_0

    # # Nix.
    nixfmt-rfc-style
    # nixfmt-classic
    nixd

    # Python.
    # poetry
    # python3
    # python3Packages.autopep8
    # python3Packages.black
    # python3Packages.ipython
    # python3Packages.isort
    # python3Packages.pip
    # pyright
    # python3Packages.pyflakes
    # python3Packages.pylint
    # python3Packages.pytest
    # python3Packages.setuptools

    # OCaml.
    opam

    # Rust.
    cargo-edit
    cargo-expand
    cargo-generate
    cargo-license
    cargo-modules
    cargo-outdated
    cargo-release
    cargo-tarpaulin
    cargo-udeps
    cargo-watch
    sqlx-cli
    cargo-audit

    # Scala.
    # ammonite
    sbt
    scala-cli

    # Scheme.
    guile

    # Shell.
    shellcheck
    shfmt

    # SQL.
    sqls
    sqlfluff
    sqlint

    # Text.
    nodePackages.unified-language-server

    # TLA+
    # tlaps
    # tlaplus
    # tla-toolbox

    # Web.
    html-tidy
  ];

  home.file.".ghc/ghci.conf".text = ''
    :set prompt "λ> "
    :set prompt-cont "λ| "

    :seti -XDataKinds
    :seti -XTypeApplications
    :seti -XTypeOperators
  '';

  home.file.".guile".text = ''
    (use-modules (texinfo reflection)) ;; help
    (use-modules (ice-9 readline))
    (activate-readline)
  '';
}
