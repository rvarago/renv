{
  description = "My Nix env";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      nixpkgs,
      home-manager,
      ...
    }:
    let
      settings = import ./nixpkgs/settings.nix;

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      makeConfig =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = {
            inherit settings;
            isDarwin = pkgs.stdenv.isDarwin;
          };
          modules = [
            (import ./nixpkgs/home.nix)
          ];
        };
    in
    {
      homeConfigurations = builtins.listToAttrs (
        builtins.map (system: {
          name = "${settings.user}@${system}";
          value = makeConfig system;
        }) systems
      );
    };
}
