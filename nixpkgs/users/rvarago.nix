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

  xdg = {
    enable = true;
    mime.enable = true;
  };

  xsession.enable = true;

  targets.genericLinux.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # CLI tools.
    bat
    bc
    editorconfig-core-c
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
    tealdeer
    texlive.combined.scheme-small
    tokei
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
    strace
    valgrind
    wireshark
    patchelf

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
    ../modules/langs
    ../modules/neovim
    ../modules/tmux
    ../modules/vcs
    ../modules/vscode

  ];

}
