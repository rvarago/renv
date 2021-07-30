{ pkgs, ... }: {

  programs.git = {
    enable = true;

    userEmail = "rafael.varago@gmail.com";
    userName = "Rafael Varago";

    ignores = [
      # local stuff
      ".local"
      # emacs
      "\\#*#"
      # vscode
      ".vscode"
    ];

    extraConfig = {
      diff = { tool = "meld"; };
      meld = { tool = "meld"; };

      init = { defaultBranch = "main"; };

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
      br = "branch";
      ci = "commit";
      cia = "commit --amend";
      cif = "commit --fixup";
      co = "checkout";
      lg =
        "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
      pf = "push --force-with-lease";
      ria = "rebase --interactive --autosquash";
      review = "push HEAD:refs/for/master";
      st = "status";
      vdiff = "difftool --dir-diff --no-prompt";
    };
  };

  home.packages = [ pkgs.gitAndTools.gitflow ];
}
