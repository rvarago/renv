{ pkgs, ... }: {

  programs.tmux = {
    enable = true;

    prefix = "C-x";
    keyMode = "vi";
    customPaneNavigationAndResize = true;

    extraConfig = ''
      bind '\' split-window -h
      bind - split-window -v
      unbind '"'
      unbind %

      set -g mouse on
    '';
  };
}
