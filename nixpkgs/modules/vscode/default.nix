{ pkgs, ... }: {

  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        baccata.scaladex-search
        betterthantomorrow.calva
        coolbear.systemd-unit-file
        davidanson.vscode-markdownlint
        dbaeumer.vscode-eslint
        eamodio.gitlens
        elmtooling.elm-ls-vscode
        esbenp.prettier-vscode
        formulahendry.code-runner
        foxundermoon.shell-format
        golang.go
        haskell.haskell
        ionide.ionide-fsharp
        james-yu.latex-workshop
        justusadam.language-haskell
        kahole.magit
        matklad.rust-analyzer
        ms-azuretools.vscode-docker
        ms-dotnettools.csharp
        ms-kubernetes-tools.vscode-kubernetes-tools
        # ms-python.python
        ms-python.vscode-pylance
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
      "workbench.colorTheme" = "One Monokai";
      # "workbench.colorTheme" = "Night Owl";

      "files.watcherExclude" = {
        "**/.bloop" = true;
        "**/.metals" = true;
        "**/.ammonite" = true;
      };

      # Extensions
      "[cpp][c]" = {
        "editor.defaultFormatter" = "xaver.clang-format";
      };

      "cmake.configureOnOpen" = true;

      # Available checks: https://clang.llvm.org/extra/clang-tidy/.
      "clang-tidy.checks" = [
        "-*"
        "clang-analyzer-*"
        "readability-*"
        "cppcoreguidelines-*"
        "google-*"
      ];

      "todohighlight.keywords" = [
        {
          "text" = "WIP:";
          "color" = "#94bff3";
        }
      ];

      "go.lintTool" = "golangci-lint";
      # Enable this if things slow down significantly.
      # "go.lintFlags" = ["--fast"];

      "idris.idrisPath" = "${pkgs.idris2}/bin/idris2";
      "idris.idris2Mode" = true;

      "sql.linter.executablePath" = "${pkgs.sqlfluff}/bin/sqlfluff";
      "[sql]" = {
        "editor.defaultFormatter" = "dorzey.vscode-sqlfluff";
      };

      "[java]" = {
        "editor.defaultFormatter" = "redhat.java";
      };

      "[dockerfile]" = {
        "editor.defaultFomatter" = "ms-azuretools.vscode-docker";
      };

      "shellformat.path" = "${pkgs.shfmt}/bin/shfmt";

      "plantuml.render" = "Local";

      "nix.enableLanguageServer" = true;

      "python.analysis.typeCheckingMode" = "basic";
      "python.linting.enabled" = true;
      "python.linting.pylintEnabled" = true;
      "python.linting.pylintPath" = "${pkgs.python3Packages.pylint}/bin/pylint";
      "python.formatting.autopep8Path" = "${pkgs.python3Packages.autopep8}/bin/autopep8";
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

      # Jump
      {
        key = "alt+g";
        command = "extension.aceJump";
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
