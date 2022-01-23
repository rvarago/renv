{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages (ps:
    with ps; [
      conduit
      lens
      megaparsec
      mtl
      polysemy
      servant
      transformers
    ]);
in mkShell { buildInputs = [ ghc hlint ormolu ]; }
