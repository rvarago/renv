# renv

> "Rafael's Environment", a bunch of quick & dirty, perhaps awkward hacks tailored for my own usage.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Usage

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
λ   ./renvctl help
```

## Post-Installation

### Set Fish as Default Shell

Append to shells and then set the shell:

```bash
echo /home/$USER/.nix-profile/bin/fish >> "/etc/shells" && chsh -s /home/$USER/.nix-profile/bin/fish $USER
```

### Setup Docker

Install docker by following the steps:

- <https://docs.docker.com/engine/install/>
