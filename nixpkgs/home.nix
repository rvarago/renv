{ config, pkgs, lib, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "rvarago";
  home.homeDirectory = "/home/rvarago";
  home.sessionVariables = { EDITOR = "nvim"; };

  xdg.enable = true;
  xdg.mime.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # CLI tools
    bat
    exa
    fd
    fzf
    htop
    pandoc
    ripgrep
    socat
    tokei
    xclip
    xsv
    wget

    # Dev tools
    diff-so-fancy
    meld
    nixfmt
    shellcheck

    # Editors
    neovim

    # Fonts
    jetbrains-mono
    (nerdfonts.override { fonts = [ "Hack" "Iosevka" ]; })

    # Languages
    rustup
  ];

  programs.direnv = {
    enable = true;

    enableFishIntegration = true;
  };

  programs.git = {
    enable = true;

    userEmail = "varago.rafael@gmail.com";
    userName = "Rafael Varago";

    ignores = [ ".local" ];

    extraConfig = {
      core = {
        editor = "nvim";
        ignorecase = false;
        pager = "diff-so-fancy | less --tabs=4 -RFX";
      };

      color = {
        ui = true;
        diff-highlight = {
          oldNormal = "red bold";
          oldHighlight = "red bold 52";
          newNormal = "green bold";
          newHighlight = "green bold 22";
        };
        diff = {
          meta = 11;
          frag = "magenta bold";
          commit = "yellow bold";
          old = "red bold";
          new = "green bold";
          whitespace = "red reverse";
        };
      };
    };

    aliases = {
      ca = "commit --autosquash";
      ci = "commit";
      cif = "commit --fixup";
      co = "checkout";
      lg =
        "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      pf = "push --force-with-lease";
      ria = "rebase --interactive --autosquash";
      review = "push HEAD:refs/for/master";
      st = "status";
    };
  };

  programs.fish = {
    enable = true;

    plugins = [
      {
        name = "bass";
        src = pkgs.fetchFromGitHub {
          owner = "edc";
          repo = "bass";
          rev = "50eba266b0d8a952c7230fca1114cbc9fbbdfbd4";
          sha256 = "0ppmajynpb9l58xbrcnbp41b66g7p0c9l2nlsvyjwk6d16g4p4gy";
        };
      }

      {
        name = "foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
          sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
        };
      }

      {
        name = "neolambda";
        src = pkgs.fetchFromGitHub {
          owner = "hasanozgan";
          repo = "theme-lambda";
          rev = "9cf5825c31a1d09d37d87e681ac2fa1d771ef6d2";
          sha256 = "1aq8r27n4ifickg7my039k618d7dllknyi4g7x742hcy19zr1336";
        };
      }
    ];
  };

  services.lorri = { enable = true; };

  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        bbenoist.Nix
        brettm12345.nixfmt-vscode
        dotjoshjohnson.xml
        eamodio.gitlens
        elmtooling.elm-ls-vscode
        foxundermoon.shell-format
        golang.Go
        haskell.haskell
        matklad.rust-analyzer
        ms-python.python
        mskelton.one-dark-theme
        ms-vscode.cpptools
        redhat.java
        redhat.vscode-yaml
        scalameta.metals
        serayuzgur.crates
        skyapps.fish-vscode
        streetsidesoftware.code-spell-checker
        tamasfe.even-better-toml
        timonwong.shellcheck
        yzhang.markdown-all-in-one
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [

        {
          name = "one-monokai";
          publisher = "azemoh";
          version = "0.5.0";
          sha256 = "1rqg3np6jc9lrl9xqq8iq74y4ag3wnj5c0zv9h9ljpk5xzp4rdva";
        }

        {
          name = "terminal";
          publisher = "formulahendry";
          version = "0.0.10";
          sha256 = "0gj71xy7r82n1pic00xsi04dg7zg0dsxx000s03iq6lnz47s84gn";
        }

      ];

    userSettings = {
      "editor.lineHeight" = 20;
      "editor.fontFamily" = "JetBrains Mono";
      "editor.fontLigatures" = true;
      "editor.fontSize" = 18;
      "editor.minimap.enabled" = false;

      "terminal.integrated.fontFamily" = "Iosevka";
      "terminal.integrated.fontLigatures" = true;

      "workbench.colorTheme" = "One Monokai";
    };

  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";

}
