{ ... }:
{

  programs.tmux = {
    enable = true;

    prefix = "C-b";
    keyMode = "vi";
    customPaneNavigationAndResize = true;

    extraConfig = ''
      bind '\' split-window -h -c "#{pane_current_path}"
      bind - split-window -v -c "#{pane_current_path}"
      bind c new-window -c "#{pane_current_path}"

      unbind '"'
      unbind %

      set -g mouse on

      set -g default-terminal "tmux-256color"
    '';
  };
}
