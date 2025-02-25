{ pkgs, config, ... }:

let
  emacsOverlayRev = "93d351a5010799656026bb4ff253b0e68eab4de5";
  doomRev = "2bc052425ca45a41532be0648ebd976d1bd2e6c1";

  emacs-overlay = import (
    builtins.fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/${emacsOverlayRev}.tar.gz";
      sha256 = "sha256:0ajqaaip9a797niayhf87jwyxacb23zakn66yslv7i5cj6c07ybd";
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
    package = pkgs.emacs29;

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

      onChange = "${pkgs.writeShellScript "doom-change" ''
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        if [ ! -d "$DOOMLOCALDIR" ]; then
          ${doombin} install --force --no-hooks
        else
          ${doombin} --force sync -u --rebuild
        fi
      ''}";
    };

    "${doomdir}" = {
      source = ./doom.d;
      recursive = true;

      onChange = "${pkgs.writeShellScript "doom-config-change" ''
        export EMACSDIR="${emacsdir}"
        export DOOMDIR="${doomdir}"
        export DOOMLOCALDIR="${doomlocaldir}"
        export DOOMPROFILELOADFILE="${doomprofileloadfile}"
        ${doombin} --force sync -u --rebuild
      ''}";
    };

  };
}
