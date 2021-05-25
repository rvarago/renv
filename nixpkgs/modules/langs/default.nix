{ pkgs, lib, ... }:

let
  agda = [ (pkgs.agda.withPackages (p: [ p.standard-library ])) ];

  cpp = [ pkgs.cmake pkgs.conan ];

  elm = with pkgs.elmPackages; [
    pkgs.elmPackages.elm
    elm-analyse
    elm-format
    elm-test
  ];

  haskell = with pkgs.haskellPackages; [
    cabal-install
    hlint
    hoogle
    hpack
    implicit-hie
    stack
  ];

  idris = [ pkgs.idris2 ];

  java = [ pkgs.openjdk pkgs.maven ];

  rust = [ pkgs.rustup ];

  scala = [ pkgs.sbt ];

  langs = agda ++ cpp ++ elm ++ haskell ++ idris ++ java ++ rust ++ scala;
in {
  home.packages = langs;

  home.file.".ghc/ghci.conf".text = ''
    :set prompt λ>
  '';
  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };
}

