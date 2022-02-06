{ pkgs, ... }: {

  home.packages = with pkgs; [
    editorconfig-core-c
  ];

  imports = [
    ./emacs
    ./neovim
    ./vscode
  ];

}
