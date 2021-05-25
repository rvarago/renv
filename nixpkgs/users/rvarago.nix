{ pkgs, settings, ... }:

{
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

  home.packages = with pkgs; [
    # CLI tools
    bat
    exa
    fd
    fzf
    graphviz
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
  ];

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
    ../modules/langs
    ../modules/neovim
    ../modules/vscode

  ];

}
