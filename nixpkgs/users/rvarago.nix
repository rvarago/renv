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
    tokei
    tree
    xclip
    xdot
    xsv
    wget

    # Dev tools.
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

    # Writing.
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    aspellDicts.pt_BR
    languagetool
    texlive.combined.scheme-small

    # Fonts.
    jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

    # General tools.
    skype
    # slack
  ];

  imports = [
    ../modules/editors
    ../modules/langs
    ../modules/shell
    ../modules/vcs
  ];

}
