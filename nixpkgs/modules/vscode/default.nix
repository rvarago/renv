{ pkgs, ... }: {

  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        baccata.scaladex-search
        bbenoist.Nix
        brettm12345.nixfmt-vscode
        davidanson.vscode-markdownlint
        dotjoshjohnson.xml
        eamodio.gitlens
        elmtooling.elm-ls-vscode
        foxundermoon.shell-format
        golang.Go
        haskell.haskell
        james-yu.latex-workshop
        justusadam.language-haskell
        matklad.rust-analyzer
        ms-azuretools.vscode-docker
        ms-dotnettools.csharp
        ms-python.python
        ms-vscode.cpptools
        ocamllabs.ocaml-platform
        redhat.java
        redhat.vscode-yaml
        scalameta.metals
        scala-lang.scala
        serayuzgur.crates
        skyapps.fish-vscode
        streetsidesoftware.code-spell-checker
        tamasfe.even-better-toml
        timonwong.shellcheck
        tomoki1207.pdf
        # vadimcn.vscode-lldb FIXME: liblldb is null.
        yzhang.markdown-all-in-one
        xaver.clang-format
      ] ++ import ./market_extensions.nix {
        inherit (pkgs.vscode-utils) extensionFromVscodeMarketplace;
      };

    userSettings = {
      "editor.lineHeight" = 20;
      "editor.fontFamily" = "JetBrains Mono";
      "editor.fontLigatures" = true;
      "editor.fontSize" = 16;
      "editor.minimap.enabled" = false;

      "terminal.integrated.fontFamily" = "Iosevka";

      "workbench.colorTheme" = "One Monokai";

      "files.watcherExclude" = {
        "**/.bloop" = true;
        "**/.metals" = true;
        "**/.ammonite" = true;
      };

      # Extensions
      "[cpp]" = { "editor.defaultFormatter" = "xaver.clang-format"; };

      "cmake.configureOnOpen" = true;

      "shellformat.path" = "${pkgs.shfmt}/bin/shfmt";

      "python.linting.enabled" = true;
      "python.linting.pylintEnabled" = true;
      "python.linting.pylintPath" = "${pkgs.python3Packages.pylint}/bin/pylint";
    };

    keybindings = [
      {
        key = "ctrl+,";
        command = "workbench.action.focusActiveEditorGroup";
        when = "terminalFocus";
      }
      {
        key = "ctrl+,";
        command = "workbench.action.terminal.focus";
        when = "!terminalFocus";
      }
    ];
  };
}
