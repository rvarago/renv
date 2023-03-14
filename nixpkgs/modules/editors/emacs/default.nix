{ pkgs, config, ... }:

let
  emacsOverlayRev = "4e9ef767be5f4fcb59cd0f908779cfc3729c1880";
  doomRev = "63586423dab6248d6e5acfc68dc4324c15f05d83";

  emacs-overlay = import (
    builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/${emacsOverlayRev}.tar.gz";
      sha256 = "1yx66ccrsaiy3i05zkq2g7si00qypy9xp9gzbs2flhg3j61n7gfp";
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
    package = pkgs.emacsUnstable;

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
        rev = doomRev;
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
