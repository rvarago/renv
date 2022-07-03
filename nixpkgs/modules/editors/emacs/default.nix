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

  services.emacs = {
    enable = true;

    client.enable = false;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;

    extraPackages = (
      epkgs:
      (
        with epkgs; [
          all-the-icons
          pdf-tools
          vterm
        ]
      )
    );
  };

  xresources.properties = {
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.font" = "-CYEL-Iosevka-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1";
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
        url = "https://github.com/hlissner/doom-emacs";
        ref = "master";
      };

      onChange = "${pkgs.writeShellScript "doom-change" ''
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        if [ ! -d "$DOOMLOCALDIR" ]; then
          ${doombin} install --no-hooks
        else
          ${doombin} clean
          ${doombin} sync -u
        fi
      ''}";
    };

    ".doom.d" = {
      source = ./doom.d;
      recursive = true;

      onChange = "${pkgs.writeShellScript "doom-local-change" ''
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        ${doombin} sync
        # To recompile Run ${doombin} build -r
      ''}";
    };

  };
}
