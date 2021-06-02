{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    # Agda
    (agda.withPackages (p: [ p.standard-library ]))

    # C/C++
    clang-tools
    cmake
    conan

    # Clojure
    leiningen

    # Elm
    elmPackages.elm
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm-test

    # Haskell
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.stack
    haskellPackages.haskell-language-server

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
    opam

    # Rust
    cargo-audit
    cargo-edit
    cargo-expand
    cargo-license
    cargo-tarpaulin
    cargo-udeps
    cargo-watch
    rustup
    rust-analyzer

    # Scala
    sbt
  ];

  home.file.".ghc/ghci.conf".text = ''
    :set prompt λ>
  '';
  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };
}

