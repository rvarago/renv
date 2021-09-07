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
        all-the-icons-dired
        all-the-icons-ivy
        auto-dim-other-buffers
        ccls
        centaur-tabs
        cmake-mode
        command-log-mode
        company
        company-box
        company-coq
        company-quickhelp
        # company-yasnippet
        counsel
        counsel-projectile
        cpp-auto-include
        # dap-java
        dap-mode
        direnv
        diff-hl
        dockerfile-mode
        doom-modeline
        elpy
        flycheck
        flymake-shellcheck
        # flyspell
        go-mode
        helm-ag
        helpful
        hl-todo
        ini-mode
        ivy
        ivy-rich
        lsp-haskell
        lsp-ivy
        lsp-java
        lsp-metals
        lsp-mode
        lsp-treemacs
        lsp-ui
        magit
        move-text
        nix-mode
        projectile
        proof-general
        protobuf-mode
        rainbow-delimiters
        # recentf
        rg
        ripgrep
        rustic
        sbt-mode
        scala-mode
        shackle
        sqlformat
        smartparens
        swiper
        tide
        treemacs
        treemacs-all-the-icons
        treemacs-magit
        treemacs-projectile
        undo-tree
        use-package
        use-package-chords
        vterm
        vterm-toggle
        which-key
        yaml-mode
        yang-mode
        yasnippet
        yasnippet-snippets
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
