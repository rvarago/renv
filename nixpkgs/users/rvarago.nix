{ pkgs, settings, ... }:

let
  user = settings.user;
  home = "/home/${user}";
in {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = home;
  home.sessionVariables = {
    EDITOR = "nvim";
    JAVA_HOME = "${home}/.nix-profile/lib/openjdk/";
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
    imagemagick
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

    # Cloud
    docker-compose
    helm
    kubectl
    kubectx
    minikube

    # Fonts
    jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

    # Languages & tooling
    nixfmt
    shellcheck
    shfmt
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
