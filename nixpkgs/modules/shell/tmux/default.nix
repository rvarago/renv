{ pkgs, ... }: {

  programs.tmux = {
    enable = true;

    prefix = "C-b";
    keyMode = "vi";
    customPaneNavigationAndResize = true;

    extraConfig = ''
      bind '\' split-window -h
      bind - split-window -v
      unbind '"'
      unbind %

      set -g mouse on

      set -g default-terminal "tmux-256color"
    '';
  };
}
