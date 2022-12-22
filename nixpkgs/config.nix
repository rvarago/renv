{ pkgs }: {

  allowUnfree = true;

  allowBroken = true;

  permittedInsecurePackages = [
    "qtwebkit-5.212.0-alpha4"
  ];
}

