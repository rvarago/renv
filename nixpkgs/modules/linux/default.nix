{ pkgs, ... }: {

  xdg = {
    enable = true;
    mime.enable = true;
  };

  xsession.enable = true;

  targets.genericLinux.enable = true;

}
