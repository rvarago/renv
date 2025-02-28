{ pkgs, ... }:
{

  xdg = {
    enable = true;
    mime.enable = true;
  };

  xsession.enable = true;

  targets.genericLinux.enable = true;

  home.packages = with pkgs; [
    # unsupported on macos
    racket
    grip
    libvterm # for Emacs Vterm
    xdot
    # broken on macos
    valgrind
  ];
}
