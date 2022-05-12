{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  ruby = ruby_3_0.withPackages (ps:
    with ps; [
      rubocop
      solargraph
    ]);
in
mkShell { buildInputs = [ ruby ]; }
