{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    # Agda
    (agda.withPackages (p: [ p.standard-library ]))

    # C/C++
    ccls
    clang-tools
    cmake
    conan

    # Clojure
    leiningen

    # Coq
    coq

    # Elm
    elmPackages.elm
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm-test

    # Go
    go
    go-outline
    gopls
    gotests

    # Haskell
    ghc
    haskellPackages.cabal-install
    haskellPackages.haskell-language-server
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.stack

    # Idris
    idris2

    # Java
    openjdk
    maven

    # Python
    python3
    python3Packages.pip

    # OCaml
    ocamlformat
    ocamlPackages.ocaml-lsp
    ocamlPackages.utop
    opam

    # Rust
    cargo-audit
    cargo-edit
    cargo-expand
    cargo-license
    cargo-modules
    cargo-tarpaulin
    cargo-udeps
    cargo-watch
    rustup
    rust-analyzer

    # Scala
    sbt

    # Scheme
    racket
    guile
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
