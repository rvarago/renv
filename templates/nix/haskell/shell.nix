{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages
    (ps: with ps; [ conduit lens mtl polysemy ]);
in mkShell { buildInputs = [ ghc hlint ormolu ]; }
