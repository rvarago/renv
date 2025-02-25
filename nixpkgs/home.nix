{
  pkgs,
  lib,
  ...
}:

let
  settings = import ./settings.nix;
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = settings.user;
  home.homeDirectory = "/${if pkgs.stdenv.isDarwin then "Users" else "home"}/${settings.user}";

  home.sessionVariables = {
    USER_FULL_NAME = settings.userFullName;
    USER_EMAIL = settings.userEmail;
  };

  home.packages = with pkgs; [
    # CLI tools.
    bat
    bc
    eza
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
    xan
    wget
    #wkhtmltopdf

    # Dev tools.
    minicom
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
    #minikube
    hadolint
    #terraform

    # Writing.
    aspell
    #aspellDicts.de
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    aspellDicts.pt_BR
    languagetool
    #texlive.combined.scheme-small

    # Fonts.
    nerd-fonts.iosevka
    nerd-fonts.jetbrains-mono
    jetbrains-mono
  ];

  fonts.fontconfig.enable = true;

  imports = [
    ./modules/emacs
    ./modules/neovim.nix

    ./modules/tmux.nix
    ./modules/kitty.nix
    ./modules/fish.nix

    (import ./modules/git.nix { inherit pkgs settings; })

    ./modules/langs.nix

    ./modules/linux.nix

  ] ++ lib.optional (builtins.pathExists ./modules/ephemeral.nix) ./modules/ephemeral.nix;

}
