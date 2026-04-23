{ pkgs, config, ... }:

let
  emacsOverlayRev = "568d47313336d3a10bd3e27aad32c399f0c8cde6";
  doomRev = "57818a6da90fbef39ff80d62fab2cd319496c3b9";

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
    # tree-sitter
    # (tree-sitter-grammars.with-grammars (grammars: with grammars; [
    #   tree-sitter-typescript
    #   tree-sitter-tsx
    #   tree-sitter-javascript
    #   tree-sitter-python
    #   tree-sitter-rust
    #   tree-sitter-go
    #   tree-sitter-nix
    #   tree-sitter-bash
    #   tree-sitter-json
    #   tree-sitter-yaml
    #   tree-sitter-html
    #   tree-sitter-css
    # ]))
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
        treesit-grammars.with-all-grammars
        # tsc
        # tree-sitter-langs
        # tree-sitter
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
    # TREE_SITTER_LANGUAGE_DIR = "${pkgs.tree-sitter-grammars.with-grammars (g: with g; [tree-sitter-typescript tree-sitter-tsx tree-sitter-javascript tree-sitter-python tree-sitter-rust tree-sitter-go tree-sitter-nix tree-sitter-bash tree-sitter-json tree-sitter-yaml tree-sitter-html tree-sitter-css])}/lib";
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
