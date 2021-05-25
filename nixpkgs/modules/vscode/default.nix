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
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [

        # sha256 = lib.fakeSha256

        {
          name = "bash-ide-vscode";
          publisher = "mads-hartmann";
          version = "1.11.0";
          sha256 = "1hq41fy2v1grjrw77mbs9k6ps6gncwlydm03ipawjnsinxc9rdkp";
        }

        {
          name = "better-comments";
          publisher = "aaron-bond";
          version = "2.1.0";
          sha256 = "0kmmk6bpsdrvbb7dqf0d3annpg41n9g6ljzc1dh0akjzpbchdcwp";
        }

        {
          name = "cmake";
          publisher = "twxs";
          version = "0.0.17";
          sha256 = "11hzjd0gxkq37689rrr2aszxng5l9fwpgs9nnglq3zhfa1msyn08";
        }

        {
          name = "cmake-tools";
          publisher = "ms-vscode";
          version = "1.7.3";
          sha256 = "0jisjyk5n5y59f1lbpbg8kmjdpnp1q2bmhzbc1skq7fa8hj54hp9";
        }

        {
          name = "doxdocgen";
          publisher = "cschlosser";
          version = "1.3.1";
          sha256 = "17cgkf2h1bg61axsgf033a22rncpc68cii4z7381ag9m1ssgi0kz";
        }

        {
          name = "one-monokai";
          publisher = "azemoh";
          version = "0.5.0";
          sha256 = "1rqg3np6jc9lrl9xqq8iq74y4ag3wnj5c0zv9h9ljpk5xzp4rdva";
        }

        {
          name = "nix-env-selector";
          publisher = "arrterian";
          version = "1.0.7";
          sha256 = "0mralimyzhyp4x9q98x3ck64ifbjqdp8cxcami7clvdvkmf8hxhf";
        }

        {
          name = "rest-client";
          publisher = "humao";
          version = "0.24.5";
          sha256 = "1hj294nsmlzvhbvwv4wyf7mgfw64q4pgkjzzgyjfc26pzyaxb4bn";
        }

        {
          name = "terminal";
          publisher = "formulahendry";
          version = "0.0.10";
          sha256 = "0gj71xy7r82n1pic00xsi04dg7zg0dsxx000s03iq6lnz47s84gn";
        }

        {
          name = "vscode-direnv";
          publisher = "Rubymaniac";
          version = "0.0.2";
          sha256 = "1gml41bc77qlydnvk1rkaiv95rwprzqgj895kxllqy4ps8ly6nsd";
        }

      ];

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
