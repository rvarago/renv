let

  sources = import ./nix/sources.nix;

  nixpkgs = sources."nixpkgs-unstable";

  pkgs = import nixpkgs { };

in
pkgs.mkShell rec {

  name = "home-manager-shell";

  buildInputs = with pkgs; [
    niv
    (import sources.home-manager { inherit pkgs; }).home-manager
  ];

  shellHook = ''
    export NIXPKGS_ALLOW_UNFREE=1
    export NIX_PATH="nixpkgs=${nixpkgs}:home-manager=${sources."home-manager"}"
    export HOME_MANAGER_CONFIG="./nixpkgs/home.nix"
  '';

}
