{ pkgs, config, ... }:

let
  emacs-overlay = import (
    builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }
  );

  emacsdir = "${config.home.homeDirectory}/.emacs.d";
  doomprivatedir = "${config.home.homeDirectory}/.doom.d";
  doombin = "${emacsdir}/bin/doom";
in
{
  # Emacs.
  nixpkgs.overlays = [ emacs-overlay ];

  home.packages = with pkgs; [
    python3 # Treemacs requires python3
    emacs-all-the-icons-fonts
  ];

  services.emacs.enable = true;

  programs.emacs = {
    enable = true;
    package = pkgs.emacs; # pkgs.emacsGit for 28.x.

    extraPackages = (
      epkgs:
        (
          with epkgs; [
            all-the-icons
            vterm
          ]
        )
    );
  };

  xresources.properties = {
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.font" = "JetBrains Mono-15";
  };

  # Doom.
  home.sessionPath = [ "${emacsdir}/bin" ];
  home.sessionVariables = {
    DOOMDIR = "${doomprivatedir}";
    DOOMLOCALDIR = "${doomprivatedir}/var";
  };

  home.file = {
    ".emacs.d" = {
      source = builtins.fetchGit "https://github.com/hlissner/doom-emacs";

      onChange = "${pkgs.writeShellScript "doom-change" ''
        if [ ! -d "$DOOMLOCALDIR" ]; then
          ${doombin} -y install
        else
          ${doombin} -y clean
          ${doombin} -y sync -u
        fi
      ''}";
    };

    ".doom.d" = {
      source = ./doom.d;
      recursive = true;

      onChange = "${pkgs.writeShellScript "doom-private-change" ''
        ${doombin} -y sync
      ''}";
    };

  };
}
