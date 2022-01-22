{ pkgs, settings, ... }:

let
  user = settings.user;
  home = "/home/${user}";
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = user;
  home.homeDirectory = home;
  home.sessionVariables = {
    EDITOR = "nvim";
    JAVA_HOME = "${pkgs.openjdk}/lib/openjdk";
  };

  xdg.enable = true;
  xdg.mime.enable = true;

  targets.genericLinux.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # CLI tools.
    bat
    bc
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
    rlwrap
    socat
    texlive.combined.scheme-small
    tokei
    tmux
    tree
    xclip
    xdot
    xsv
    wget

    # Dev tools.
    diff-so-fancy
    meld
    plantuml
    protobuf
    valgrind
    wireshark

    # Cloud.
    docker-compose
    helm
    kubectl
    kubectx
    minikube
    hadolint
    terraform

    # Writing
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    aspellDicts.pt_BR
    languagetool

    # Fonts.
    jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

    # General tools.
    skype
    # slack
  ];

  programs.direnv = {
    enable = true;

    nix-direnv.enable = true;

    enableBashIntegration = true;
  };

  services.lorri = { enable = true; };

  imports = [

    ../modules/emacs
    ../modules/fish
    ../modules/git
    ../modules/langs
    ../modules/neovim
    ../modules/vscode

  ];

}
