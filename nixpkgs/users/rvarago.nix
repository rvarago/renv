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
    USER_FULL_NAME = settings.userFullName;
    USER_EMAIL = settings.userEmail;
  };

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
    tealdeer
    tokei
    tree
    xclip
    xdot
    xsv
    wget
    #wkhtmltopdf

    # Dev tools.
    plantuml
    protobuf
    valgrind
    wireshark
    patchelf

    # Cloud.
    docker-compose
    helm
    kind
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
    iosevka-bin
    # (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })
    jetbrains-mono
  ];

  fonts.fontconfig.enable = true;

  imports = [
    (import ../modules/editors { inherit pkgs; })
    ../modules/langs
    ../modules/linux
    ../modules/profiles
    ../modules/shell
    (import ../modules/vcs { inherit pkgs settings; })
  ];

}
