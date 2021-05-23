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

    # Languages
    rustup
  ];

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
