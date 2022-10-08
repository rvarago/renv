# renv

> "Rafael's Environment", a bunch of quick & dirty, perhaps awkward hacks tailored for my own usage.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Usage

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
λ   ./renvctl help
```

## Pre-Installation

0. Ensure that [settings.nix](./nixpkgs/settings.nix) is correct for the environment (e.g. username matches the system)
1. Run `./renvctl check`
   - Install the missing dependencies
2. Run `./renvctl deb:install`
3. Run `./renvctl nix:install` (may need a reboot)
4. Run `./renvctl nix:update`
5. Run `./renvctl nix:cache:install`
6. Run `./renvctl home:install`
7. Run `./renvctl $LANG:install` (where `$LANG in {ocaml, lean, rust}`)

## Usage

Run `./renvctl deb:upgrade` to upgrade Debian packages, `./renvctl nix:update` to update the Nixpkg channel, and finally `./renvctl home:apply` to install all the home-manager managed packages.

## Post-Installation

### Load Environment Variables

Append to variables exposed by home-manager to the login config:

```bash
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" >> "/home/$USER/.profile"
```

### Set Fish as Default Shell

Append to shells and then set the shell:

```bash
echo /home/$USER/.nix-profile/bin/fish >> "/etc/shells" && chsh -s /home/$USER/.nix-profile/bin/fish $USER
```

### Setup Docker

Install docker by following the steps:

- <https://docs.docker.com/engine/install/>

## TODOs

- Pin everything
  - Use flakes for Nixpkg pinning
  - Pin emacs
  - Pin doomemacs
  - Pin extra emacs packages managed with straight
