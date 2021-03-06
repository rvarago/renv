{ pkgs, ... }: {

  programs.vscode = {
    enable = true;

    extensions = with pkgs.vscode-extensions;
      [
        alygin.vscode-tlaplus
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
        hashicorp.terraform
        haskell.haskell
        humao.rest-client
        ionide.ionide-fsharp
        james-yu.latex-workshop
        jnoortheen.nix-ide
        justusadam.language-haskell
        kahole.magit
        mads-hartmann.bash-ide-vscode
        ms-azuretools.vscode-docker
        ms-dotnettools.csharp
        ms-kubernetes-tools.vscode-kubernetes-tools
        # ms-python.python
        ms-python.vscode-pylance
        ms-vscode.cpptools
        ocamllabs.ocaml-platform
        redhat.java
        redhat.vscode-yaml
        ritwickdey.liveserver
        rust-lang.rust-analyzer
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
        zxh404.vscode-proto3
      ] ++ import ./market_extensions.nix {
        inherit (pkgs.vscode-utils) extensionFromVscodeMarketplace;
      };

    userSettings = {
      "editor.bracketPairColorization.enabled" = true;
      "editor.lineHeight" = 20;
      "editor.fontFamily" = "Iosevka";
      "editor.fontLigatures" = "'SS09'";
      "editor.fontSize" = 16;
      "editor.minimap.enabled" = false;
      "editor.tokenColorCustomizations" = {
        "textMateRules" = [
          {
            "scope" = "comment";
            "settings" = {
              "fontStyle" = "italic";
            };
          }
        ];
      };

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
        "editor.defaultFormatter" = "ms-azuretools.vscode-docker";
      };

      "[html][javascript][javascriptreact][json][typescript][typescriptreact][scss]" = {
        "editor.defaultFormatter" = "esbenp.prettier-vscode";
      };

      "[ruby]" = {
        "editor.defaultFormatter" = "castwide.solargraph";
      };
      "solargraph.formatting" = true;
      "solargraph.useBundler" = true;
      "solargraph.diagnostics" = true;

      "rust-analyzer.checkOnSave.command" = "check";

      "metals.showInferredType" = true;
      "metals.showImplicitArguments" = false;
      "metals.showImplicitConversionsAndClasses" = false;

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
      {
        key = "ctrl+alt+t";
        command = "idris.typeAt";
        when = "editorLangId == idris && editorTextFocus";
      }
    ];
  };
}
