{ pkgs, config, ... }:

let
  emacs-overlay = import (
    builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/2f7fff8ee668c01803cab2f0847151fdf647134e.tar.gz";
      sha256 = "0pshwldb93g88d8mh8pfqzplhady2wspa9vjbqyshnbb7h2k717s";
    }
  );

  emacsdir = "${config.home.homeDirectory}/.emacs.d";
  doomdir = "${config.home.homeDirectory}/.doom.d";
  doomlocaldir = "${config.home.homeDirectory}/.doom.local.d";
  doomprofileloadfile = "${doomlocaldir}/cache/profile-load.el";
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

    client.enable = true;
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
    EMACSDIR = "${emacsdir}";
    DOOMDIR = "${doomdir}";
    DOOMLOCALDIR = "${doomlocaldir}";
    DOOMPROFILELOADFILE = "${doomprofileloadfile}";
  };

  home.file = {
    ".emacs.d" = {
      source = builtins.fetchGit {
        url = "https://github.com/doomemacs/doom-emacs";
        rev = "d5ccac5d71c819035fa251f01d023b3f94b4fba4";
      };

      onChange = "${pkgs.writeShellScript "doom-change" ''
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        if [ ! -d "$DOOMLOCALDIR" ]; then
          ${doombin} install --force --no-hooks
        else
          ${doombin} --force clean
          ${doombin} --force sync -u
        fi
      ''}";
    };

    ".doom.d" = {
      source = ./doom.d;
      recursive = true;

      onChange = "${pkgs.writeShellScript "doom-local-change" ''
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomlocaldir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        ${doombin} --force sync
        # To recompile Run ${doombin} build -r
      ''}";
    };

  };
}
