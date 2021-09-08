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
        agda2-mode
        all-the-icons
        all-the-icons-dired
        all-the-icons-ivy
        auto-dim-other-buffers
        ccls
        centaur-tabs
        centered-window
        cmake-mode
        command-log-mode
        company
        company-box
        company-coq
        # company-yasnippet
        counsel
        counsel-projectile
        cpp-auto-include
        # dap-java
        dap-mode
        direnv
        diff-hl
        diminish
        dockerfile-mode
        doom-modeline
        flycheck
        flymake-shellcheck
        # flyspell
        fsharp-mode
        git-timemachine
        go-mode
        helm-ag
        helpful
        hl-todo
        idris-mode
        ini-mode
        ivy
        ivy-rich
        lsp-haskell
        lsp-ivy
        lsp-java
        lsp-metals
        lsp-mode
        # lsp-python-ms
        lsp-pyright
        lsp-treemacs
        lsp-ui
        magit
        move-text
        nix-mode
        # nxml-mode
        projectile
        proof-general
        protobuf-mode
        rainbow-delimiters
        # recentf
        rg
        rustic
        sbt-mode
        scala-mode
        shackle
        sqlformat
        smartparens
        swiper
        switch-window
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
        # window
        # winner
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
    "Emacs.Font" = "Iosevka-14";
  };
}
