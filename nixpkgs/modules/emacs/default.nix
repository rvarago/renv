{ pkgs, config, ... }:

let
  emacsOverlayRev = "502293ae094f7ecd604500ffb19ad35bd429311b";
  doomRev = "56ce6cc284e8f4dd0cb0704dde6694a1b8e500ed";

  emacs-overlay = import (
    builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/${emacsOverlayRev}.tar.gz";
      sha256 = "sha256:078cxqn4zcdhq2ysdb5wicnqacy1ky48yz7r7b92cgzck3qk4xdh";
    }
  );

  emacsdir = "${config.home.homeDirectory}/.emacs.d";
  doomdir = "${config.home.homeDirectory}/.doom-config.d";
  doomlocaldir = "${config.home.homeDirectory}/.doom-local.d";
  doomprofileloadfile = "${doomlocaldir}/cache/profile-load.el";
  doombin = "${emacsdir}/bin/doom";
in
{
  # Emacs.
  nixpkgs.overlays = [ emacs-overlay ];

  home.packages = with pkgs; [
    python3 # for Treemacs
    libvterm # for Vterm
  ];

  services.emacs = {
    enable = true;

    client.enable = true;
    socketActivation.enable = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs30; # IMPORTANT: Run doom build whenever upgrading major Emacs versions.

    extraPackages = (
      epkgs:
      (with epkgs; [
        pdf-tools
        nerd-icons
        vterm
      ])
    );
  };

  xresources.properties = {
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.font" = "-JB-JetBrains Mono-bold-italic-normal-*-16-*-*-*-m-0-iso10646-1";
  };

  # Doom.
  home.sessionPath = [ "${emacsdir}/bin" ];
  home.sessionVariables = {
    EMACSDIR = "${emacsdir}";
    DOOMDIR = "${doomdir}";
    DOOMLOCALDIR = "${doomlocaldir}";
    DOOMPROFILELOADFILE = "${doomprofileloadfile}";
  };

  home.file = {
    "${emacsdir}" = {
      source = builtins.fetchGit {
        url = "https://github.com/doomemacs/doom-emacs";
        rev = doomRev;
      };

      onChange = ''
        echo "Doom installation/upgrade detected! Syncing"
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        if [ ! -d "$DOOMLOCALDIR" ]; then
          ${doombin} install --force --no-hooks
        else
          ${doombin} sync
        fi
      '';
    };

    "${doomdir}" = {
      source = ./doom.d;
      recursive = true;

      onChange = ''
        echo "Doom config changes detected! Syncing" 
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        ${doombin} sync
      '';
    };

  };
}
