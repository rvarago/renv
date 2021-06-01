{ pkgs, ... }:

let
  emacs-overlay = import (builtins.fetchTarball {
    url =
      "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  });
in {
  nixpkgs.overlays = [ emacs-overlay ];
  # Treemacs requires python3
  home.packages = with pkgs; [ python3 emacs-all-the-icons-fonts ];

  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    extraPackages = (epkgs:
      (with epkgs; [
        doom-modeline
        all-the-icons
        direnv
        use-package
        use-package-chords
        magit
        company
        flycheck
        elpy
        rg
        projectile
        undo-tree
        rainbow-delimiters
        treemacs
        treemacs-projectile
        treemacs-all-the-icons
        treemacs-magit
        lsp-mode
        lsp-treemacs
        lsp-ui
        lsp-haskell
        rustic
        auto-dim-other-buffers
        zenburn-theme
        command-log-mode
        dockerfile-mode
        nix-mode
        ivy
        ivy-rich
        counsel
        counsel-projectile
        swiper
        which-key
        helpful
        yaml-mode
        helm-ag
        ini-mode
        protobuf-mode
        tide
        sqlformat
        yang-mode
      ]));
  };

  home.file = {
    ".emacs.d" = {
      source = ./emacs.d;
      recursive = true;
    };
  };

  xresources.properties = {
    "Emacs.menuBar" = false;
    "Emacs.toolBar" = false;
    "Emacs.verticalScrollBars" = false;
    "Emacs.Font" =
      "-CYEL-Iosevka-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1";
  };
}
