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
    package = pkgs.emacsGcc;
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
        cmake-font-lock
        cmake-ide
        cmake-mode
        command-log-mode
        company
        company-box
        company-coq
        # company-graphviz-dot
        # company-yasnippet
        counsel
        counsel-projectile
        cpp-auto-include
        # dap-go
        # dap-java
        dap-mode
        # dap-cpptools
        direnv
        diff-hl
        diminish
        dockerfile-mode
        docker-compose-mode
        doom-modeline
        elm-mode
        flycheck
        flycheck-clang-tidy
        flycheck-golangci-lint
        # flyspell
        flyspell-correct-ivy
        fsharp-mode
        git-timemachine
        # goto-addr
        go-mode
        google-c-style
        gradle-mode
        graphviz-dot-mode
        haskell-mode
        # haskell-cabal
        helm-ag
        helpful
        hl-todo
        # ibuffer
        ibuffer-projectile
        idris-mode
        ini-mode
        ivy
        ivy-rich
        ivy-yasnippet
        # java-mode
        json-mode
        kubernetes
        # lsp-elm
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
        markdown-mode
        markdown-preview-mode
        move-text
        nix-mode
        # nxml-mode
        pandoc-mode
        plantuml-mode
        projectile
        proof-general
        protobuf-mode
        python-mode
        rainbow-delimiters
        # recentf
        restclient
        rg
        rustic
        sbt-mode
        scala-mode
        shackle
        # sh-mode
        # sql
        smartparens
        swiper
        switch-window
        systemd
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
