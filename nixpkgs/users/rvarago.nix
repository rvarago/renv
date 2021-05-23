{ pkgs, lib, settings, ... }: {

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rvarago";
  home.homeDirectory = "/home/rvarago";
  home.sessionVariables = { EDITOR = "nvim"; };

  xdg.enable = true;
  xdg.mime.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # CLI tools
    bat
    exa
    fd
    fzf
    htop
    jq
    pandoc
    procs
    ripgrep
    socat
    tokei
    xclip
    xsv
    wget

    # Dev tools
    diff-so-fancy
    meld
    nixfmt
    shellcheck

    # Editors
    neovim

    # Fonts
    jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

    # Languages & tooling
    (agda.withPackages (p: [ p.standard-library ]))
    cmake
    conan
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.stack
    idris2
    rustup
  ];

  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };

  programs.direnv = {
    enable = true;

    enableNixDirenvIntegration = true;
    enableFishIntegration = true;
  };

  services.lorri = { enable = true; };

  imports = [

    ../programs/fish
    ../programs/git
    ../programs/vscode

  ];

}
