{ pkgs, config, ... }:

let
  emacs-overlay = import (
    builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }
  );

  emacsdir = "${config.home.homeDirectory}/.emacs.d";
  doomdir = "${config.home.homeDirectory}/.doom.d";
  doomlocaldir = "${config.home.homeDirectory}/.doom.local.d";
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
    package = pkgs.emacsUnstable;

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
    DOOMDIR = "${doomdir}";
    DOOMLOCALDIR = "${doomlocaldir}";
  };

  home.file = {
    ".emacs.d" = {
      source = builtins.fetchGit {
        # Revert fork to upstream when https://github.com/hlissner/doom-emacs/pull/6038 gets accepted, if at all.
        url = "https://github.com/rvarago/doom-emacs";
        ref = "develop";
        rev = "abaf908da9f3f5720bbe2a59f8f5c9c931519cea";
      };

      onChange = "${pkgs.writeShellScript "doom-change" ''
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        if [ ! -d "$DOOMLOCALDIR" ]; then
          yes | ${doombin} -y install --no-hooks
        else
          ${doombin} -y clean
          ${doombin} -y sync -u
        fi
      ''}";
    };

    ".doom.d" = {
      source = ./doom.d;
      recursive = true;

      onChange = "${pkgs.writeShellScript "doom-local-change" ''
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        ${doombin} -y sync
      ''}";
    };

  };
}
