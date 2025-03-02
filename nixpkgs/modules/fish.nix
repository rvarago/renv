{ pkgs, ... }:
{

  programs.fish = {
    enable = true;

    shellInit = ''
      set fish_greeting
    '';

    interactiveShellInit = ''
      # It seems to interact badly with direnv somehow.
      # if type -q tmux
      # and not set -q TMUX
      #     tmux # Don't `exec` so I can kill tmux without killing the terminal.
      # end

      # if type -q opam
      #   eval (opam env)
      # end
    '';

    shellAliases = {
      g = "git";
      cdp = "cd $(git rev-parse --show-toplevel)";

      # t = "tmux";

      clojurew = "rlwrap clojure";
      idris2w = "rlwrap idris2";
      sbclw = "rlwrap sbcl";

      ei = "emacsclient -nw";
      gg = "ei --eval '(magit-status)'";

      nrepl = "nix repl";
      ndev = "nix develop";
      nbuild = "nix build";
      nrun = "nix run";
    };

    functions = {
      d2h = "echo \"obase=16; ibase=10; $argv\" | bc";

      h2d = "echo \"obase=10; ibase=16; $argv\" | bc";

      t = ''
        set temp_dir (mktemp -d /tmp/$argv-XXXXXX)
        pushd $temp_dir
        echo "switched to: $temp_dir"
      '';
    };

    plugins = [
      {
        name = "bass";
        src = pkgs.fetchFromGitHub {
          owner = "edc";
          repo = "bass";
          rev = "f3a547b0239cf8529d35c1922dd242bacf751d3b";
          sha256 = "sha256-3mFlFiqGfQ+GfNshwKfhQ39AuNMdt8Nv2Vgb7bBV7L4=";
        };
      }

      {
        name = "bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "816c66df34e1cb94a476fa6418d46206ef84e8d3";
          sha256 = "sha256-35xXBWCciXl4jJrFUUN5NhnHdzk6+gAxetPxXCv4pDc=";
        };
      }

      {
        name = "cd";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-cd";
          rev = "5d76c4f9589f5d43314aff2fa06499eb6b9078fe";
          sha256 = "1sppib3gw7jn3c3cc7n0k8mp74y3dni800h58ndw3fm5vdkfqb03";
        };
      }

      {
        name = "foreign-env";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-foreign-env";
          rev = "3ee95536106c11073d6ff466c1681cde31001383";
          sha256 = "sha256-vyW/X2lLjsieMpP9Wi2bZPjReaZBkqUbkh15zOi8T4Y=";
        };
      }

      {
        name = "neolambda";
        src = pkgs.fetchFromGitHub {
          owner = "hasanozgan";
          repo = "theme-lambda";
          rev = "a7cb6dbaee9e9dcbe7fea02b92fc85fb2d278869";
          sha256 = "sha256-GsLcRU6ky0GSdrtmb22RZpY4r9iRVChH64LzV46Bgbk=";
        };
      }
    ];
  };
}
