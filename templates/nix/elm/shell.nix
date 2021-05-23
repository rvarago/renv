{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = with elmPackages; [

    elm
    elm-analyse
    elm-format
    elm-test

  ];
}
