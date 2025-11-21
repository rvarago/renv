{ pkgs, settings, ... }:
{

  programs.git = {
    enable = true;

    settings = {

      user = {
        name = settings.userFullName;
        email = settings.userEmail;
      };

      github = {
        user = settings.githubUser;
      };

      diff = {
        algorithm = "histogram";
        colorMoved = "plain";
        mnemonicPrefix = true;
        renames = true;
        tool = "meld";
      };

      merge = {
        tool = "meld";
        conflictStyle = "zdiff3";
      };

      init = {
        defaultBranch = "main";
      };

      core = {
        ignorecase = false;
        pager = "diff-so-fancy | less --tabs=4 -RFX";
      };

      fetch = {
        prune = true;
        pruneTags = true;
        all = true;
      };

      commit.verbose = true;

      tag.sort = "version:refname";

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

      alias = {
        a = "add";
        ap = "add -p";

        b = "branch";
        bd = "branch -D";
        bv = "branch -vv";

        c = "commit";
        cm = "commit -m";
        ca = "commit --amend";
        cam = "commit --amend -m";
        ce = "commit --amend --no-edit";
        cen = "commit --amend --no-edit --date now";
        cf = "commit --fixup";

        m = "merge";
        mf = "merge --ff";

        y = "cherry-pick";

        co = "checkout";
        cob = "checkout -b";

        f = "fetch";
        fp = "fetch --prune";

        s = "status";
        sh = "show";

        z = "stash";
        zp = "stash push";
        zg = "stash pop";

        r = "rebase";
        ri = "rebase --interactive";
        ria = "rebase --interactive --autosquash";

        x = "reset";
        xs = "reset --soft";
        xh = "reset --hard";

        d = "diff";
        ds = "diff --stat";
        dc = "diff --cached";

        df = "difftool --dir-diff --no-prompt";

        p = "push";
        pu = "push -u";
        pushme = "!f() { git push -u $1 $(git rev-parse --abbrev-ref HEAD); }; f";
        pf = "push --force-with-lease";

        undo = "reset --soft HEAD~";

        bsyncf = "!f() { git rev-parse --abbrev-ref --symbolic-full-name @{u} | git reset --hard; }; f";

        greview = "push HEAD:refs/for/master";

        l = "log --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all";

        primary = "!f() { git symbolic-ref --short refs/remotes/origin/HEAD | sed -e 's/origin\\///'; }; f";
        trim = "!f() { git for-each-ref --format '%(upstream:track) %(refname:lstrip=2)' refs/heads | awk '/^\\[gone\\]/{print $2}' | xargs -I{} git branch -D {}; }; f";
        sync = "!f() { git fetch -p && git checkout $(git primary) && git merge --ff; }; f";
        asnew = "!f() { git sync && git trim; }; f }";
        start = "!f() { git fetch && git checkout -b $1 $(git primary); }; f";
        done = "!f() { git sync && git branch -D @{-1}; }; f";
        review = "!f() { git fetch origin $1 && git checkout origin/$1; }; f";

      };

    };

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
      ".envrc"
      "compile_commands.json"
      ".cache"

      # languages
      ".metals"
    ];

  };

  home.packages = with pkgs; [
    commitizen
    diff-so-fancy
    gitflow
    git-lfs
    meld
  ];
}
