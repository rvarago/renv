{ pkgs, ... }: {

  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        baccata.scaladex-search
        bbenoist.nix
        brettm12345.nixfmt-vscode
        davidanson.vscode-markdownlint
        dotjoshjohnson.xml
        eamodio.gitlens
        elmtooling.elm-ls-vscode
        foxundermoon.shell-format
        golang.go
        haskell.haskell
        james-yu.latex-workshop
        justusadam.language-haskell
        kahole.magit
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
        usernamehw.errorlens
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

      # "workbench.colorTheme" = "Zenburn";
      # "workbench.colorTheme" = "Visual Studio Dark";
      "workbench.colorTheme" = "Night Owl";

      "files.watcherExclude" = {
        "**/.bloop" = true;
        "**/.metals" = true;
        "**/.ammonite" = true;
      };

      # Extensions
      "[cpp]" = { "editor.defaultFormatter" = "xaver.clang-format"; };

      "cmake.configureOnOpen" = true;

      "idris.idrisPath" = "${pkgs.idris2}/bin/idris2";
      "idris.idris2Mode" = true;

      "[java]" = { "editor.defaultFormatter" = "redhat.java"; };

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
      {
        key = "ctrl+l";
        command = "codelens.showLensesInCurrentLine";
      }

      # Idris
      {
        key = "ctrl+alt+a";
        command = "idris.addClause";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+b";
        command = "idris.browseNamespace";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+c";
        command = "idris.caseSplit";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+d";
        command = "idris.docsForSelection";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+g";
        command = "idris.generateDef";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+i";
        command = "idris.interpretSelection";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+m";
        command = "idris.makeCase";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+l";
        command = "idris.makeLemma";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+w";
        command = "idris.makeWith";
        when = "editorLangId == idris && editorTextFocus";
      }
      {
        key = "ctrl+alt+p";
        command = "idris.proofSearch";
        when = "editorLangId == idris && editorTextFocus";
      }
    ];
  };
}
