{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  ghc = haskellPackages.ghcWithPackages
    (ps: with ps; [ conduit lens mtl polysemy servant ]);
in mkShell { buildInputs = [ ghc hlint ormolu ]; }
