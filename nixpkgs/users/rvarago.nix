{ pkgs, lib, settings, ... }:

let
  agda-lang = with pkgs; [ (agda.withPackages (p: [ p.standard-library ])) ];

  cpp-lang = with pkgs; [ cmake conan ];

  elm-lang = with pkgs.elmPackages; [ elm elm-analyse elm-format elm-test ];

  haskell-lang = with pkgs.haskellPackages; [
    cabal-install
    hlint
    hoogle
    hpack
    implicit-hie
    stack
  ];

  idris-lang = with pkgs; [ idris2 ];

  java-lang = with pkgs; [ openjdk maven ];

  rust-lang = with pkgs; [ rustup ];

  scala-lang = with pkgs; [ sbt ];

  langs = agda-lang ++ cpp-lang ++ elm-lang ++ haskell-lang ++ idris-lang
    ++ java-lang ++ rust-lang ++ scala-lang;
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

      # Fonts
      jetbrains-mono
      (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

      # Languages & tooling
      nixfmt
      shellcheck
    ] ++ langs;

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

    ../modules/fish
    ../modules/git
    ../modules/neovim
    ../modules/vscode

  ];

  home.file.".ghc/ghci.conf".text = ''
    :set prompt λ>
  '';

}
