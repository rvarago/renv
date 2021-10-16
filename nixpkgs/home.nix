{ config, pkgs, lib, ... }:

let
  settings = import ./settings.nix;
  user = ./users + "/${settings.user}.nix";
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [ (import user { inherit pkgs lib settings; }) ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
