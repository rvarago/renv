{ ... }: {

  programs.direnv = {
    enable = true;

    nix-direnv.enable = true;

    enableBashIntegration = true;
  };

  services.lorri = {
    enable = true;
  };

  imports = [
    ./fish
  ];

}
