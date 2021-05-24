{ pkgs, lib, settings, ... }:

let

  elm = with pkgs; [

    elmPackages.elm
    elmPackages.elm-analyse
    elmPackages.elm-format
    elmPackages.elm-test

  ];

  haskell = with pkgs; [

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.hoogle
    haskellPackages.hpack
    haskellPackages.implicit-hie
    haskellPackages.stack

  ];

in {

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rvarago";
  home.homeDirectory = "/home/rvarago";
  home.sessionVariables = {
    EDITOR = "nvim";
    JAVA_HOME = "/home/rvarago/.nix-profile/lib/openjdk/";
  };

  xdg.enable = true;
  xdg.mime.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs;
    [
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
      tree
      xclip
      xsv
      wget

      # Dev tools
      diff-so-fancy
      meld
      valgrind

      # Editors
      neovim

      # Fonts
      jetbrains-mono
      (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

      # Languages & tooling
      (agda.withPackages (p: [ p.standard-library ]))
      cmake
      conan
      idris2
      openjdk
      maven
      nixfmt
      rustup
      sbt
      shellcheck
    ] ++ elm ++ haskell;

  home.file.".stack/config.yaml".text =
    lib.generators.toYAML { } { nix.enable = true; };

  programs.direnv = {
    enable = true;

    enableNixDirenvIntegration = true;

    enableBashIntegration = true;
    enableFishIntegration = true;
  };

  services.lorri = { enable = true; };

  imports = [

    ../programs/fish
    ../programs/git
    ../programs/vscode

  ];

}
