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
        all-the-icons
        auto-dim-other-buffers
        centaur-tabs
        command-log-mode
        company
        company-coq
        counsel
        counsel-projectile
        dap-mode
        direnv
        diff-hl
        dockerfile-mode
        doom-modeline
        elpy
        flycheck
        helm-ag
        helpful
        ini-mode
        ivy
        ivy-rich
        lsp-haskell
        lsp-mode
        lsp-treemacs
        lsp-ui
        magit
        nix-mode
        projectile
        proof-general
        protobuf-mode
        rainbow-delimiters
        rg
        rustic
        shackle
        sqlformat
        swiper
        tide
        treemacs
        treemacs-all-the-icons
        treemacs-magit
        treemacs-projectile
        undo-tree
        use-package
        use-package-chords
        which-key
        yaml-mode
        yang-mode
        zenburn-theme
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
    "Emacs.Font" =
      "-CYEL-Iosevka-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1";
  };
}
