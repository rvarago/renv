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
      # "aarch64-linux" # 64-bit ARM Linux
      # "x86_64-darwin" # 64-bit Intel macOS
      # "aarch64-darwin" # 64-bit ARM macOS
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      settings = import ./nixpkgs/settings.nix;
    in
    {
      homeConfigurations = {
        ${settings.user} = home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          extraSpecialArgs = { inherit settings; };

          modules = [
            ./nixpkgs/home.nix
          ];

        };
      };
    };
}
