{ pkgs, lib, ... }: {

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

      "workbench.colorTheme" = "One Monokai";
    };

  };
}
