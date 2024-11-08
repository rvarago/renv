# renv

> "Rafael's Environment", a bunch of quick & dirty, perhaps awkward hacks tailored for my own usage.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Introduction

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
λ   ./renvctl help
```

## Pre-Installation

0. Ensure that [settings.nix](./nixpkgs/settings.nix) is correct for the environment (e.g. username matches the system, email)
   1. Run `./renvctl home:configure`
1. Run `./renvctl check`
   - Install the missing dependencies
2. Run `./renvctl deb:install`
3. Run `./renvctl nix:install` (may need a reboot)
4. Run `./renvctl nix:update`
5. Run `./renvctl nix:cache:install`
6. (Optional) Run `./renvctl vscode:install`
7. (Optional) Run `./renvctl $LANG:install` (where `$LANG in {ocaml, lean, rust}`)

## Usage

- `./renvctl customise:new` to start a new non-persistent customisation file
- `./renvctl home:apply` to install all home-manager managed packages
- `./renvctl deb:upgrade` to upgrade Debian packages
- `./renvctl nix:update` to update nixpkgs

## Post-Installation

### Load Environment Variables

Append to variables exposed by home-manager to the login config:

```bash
echo ". $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh" >> /home/$USER/.profile
```

### Set Fish as Default Shell

Append to shells and then set the shell:

```bash
export U=$USER
echo /home/$U/.nix-profile/bin/fish >> "/etc/shells" && chsh -s /home/$U/.nix-profile/bin/fish $U
```

### Setup Docker

Install docker by following the steps:

- <https://docs.docker.com/engine/install/>

## TODOs
