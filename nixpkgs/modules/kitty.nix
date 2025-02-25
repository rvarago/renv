{ pkgs, ... }:
{

  programs.kitty = {
    enable = true;
    package = pkgs.hello; # We install kitty outside of Nix due to OpenGL https://github.com/NixOS/nixpkgs/issues/239977.

    font = {
      name = "Iosevka NF ExtraBold";
      size = 14;
    };

    themeFile = "Zenburn";

    extraConfig = ''
      shell tmux
      map kitty_mod+enter launch --cwd=current --type=window
      map cmd+enter       launch --cwd=current --type=window
      map kitty_mod+t     launch --cwd=current --type=tab
      map cmd+t           launch --cwd=current --type=tab
    '';
  };
}
