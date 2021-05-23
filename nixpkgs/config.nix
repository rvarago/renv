{ pkgs }: {

  # FIXME: https://github.com/nix-community/home-manager/issues/1702.
  packageOverrides = pkgs: { fish-foreign-env = pkgs.fishPlugins.foreign-env; };

  allowUnfree = true;
}

