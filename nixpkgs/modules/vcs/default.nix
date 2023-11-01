{ pkgs, settings, ... }: {

  programs.git = {
    enable = true;

    userName = settings.userFullName;
    userEmail = settings.userEmail;

    ignores = [
      # local stuff
      ".local"
      # emacs
      "\\#*#"
      # vscode
      ".vscode"
      # jetbrains
      ".idea"
      # direnv
      ".direnv"

      # languages
      ".metals"
    ];

    extraConfig = {
      github = {
        user = settings.githubUser;
      };

      diff = { tool = "meld"; };
      merge = {
        tool = "meld";
        conflictStyle = "diff3";
      };

      init = { defaultBranch = "main"; };

      core = {
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
      a = "add";
      ap = "add -p";

      b = "branch";
      bv = "branch -vv";

      c = "commit";
      ca = "commit -a";
      cm = "commit -m";
      cam = "commit -a -m";
      cf = "commit --fixup";
      m = "commit --amend";
      mf = "commit --amend --no-edit";

      cp = "cherry-pick";

      co = "checkout";
      cob = "checkout -b";

      s = "status";
      sh = "show";

      ri = "rebase --interactive";
      ria = "rebase --interactive --autosquash";

      z = "reset";
      zh = "reset --hard";

      d = "diff";
      ds = "diff --stat";
      dc = "diff --cached";

      df = "difftool --dir-diff --no-prompt";

      pf = "push --force-with-lease";
      greview = "push HEAD:refs/for/master";

      lg =
        "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";
    };
  };

  home.packages = with pkgs; [
    commitizen
    diff-so-fancy
    gitAndTools.gitflow
    git-lfs
    meld
  ];
}
