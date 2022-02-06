{ pkgs, ... }: {

  programs.fish = {
    enable = true;

    shellInit = ''
      set fish_greeting

      if type -q opam
        eval (opam env)
      end
    '';

    shellAliases = {
      clojurew = "rlwrap clojure";

      guilew = "rlwrap guile";

      idris2w = "rlwrap idris2";

      nr = "nix-shell --run fish";

      ei = "emacs -nw";

      magit = "ei --eval '(magit)'";
    };

    functions = {
      d2h = "echo \"obase=16; ibase=10; $argv\" | bc";

      h2d = "echo \"obase=10; ibase=16; $argv\" | bc";
    };

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
        name = "bang-bang";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-bang-bang";
          rev = "f969c618301163273d0a03d002614d9a81952c1e";
          sha256 = "1r3d4wgdylnc857j08lbdscqbm9lxbm1wqzbkqz1jf8bgq2rvk03";
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
}
