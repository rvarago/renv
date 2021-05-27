{ pkgs, lib, ... }:

{

  home.packages = with pkgs; [
    (agda.withPackages (p: [ p.standard-library ]))

    clang-tools
    cmake
    conan

    elmPackages.elm
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm-test

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.stack

    idris2

    openjdk
    maven

    python3
    python3Packages.pip

    cargo-audit
    cargo-edit
    cargo-license
    cargo-tarpaulin
    rustup
    rust-analyzer

    sbt
  ];

  home.file.".ghc/ghci.conf".text = ''
    :set prompt λ>
  '';
  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };
}

