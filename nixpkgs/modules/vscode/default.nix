{ pkgs, ... }: {

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
        justusadam.language-haskell
        matklad.rust-analyzer
        ms-python.python
        ms-vscode.cpptools
        redhat.java
        redhat.vscode-yaml
        scalameta.metals
        scala-lang.scala
        serayuzgur.crates
        skyapps.fish-vscode
        streetsidesoftware.code-spell-checker
        tamasfe.even-better-toml
        timonwong.shellcheck
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

      "cmake.configureOnOpen" = true;

      "shellformat.path" = "${pkgs.shfmt}/bin/shfmt";
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
