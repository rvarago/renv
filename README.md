# renv

> "Rafael's Environment", a bunch of quick & dirty, perhaps awkward hacks tailored for my own usage.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Usage

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
λ   ./renvctl help
Manage my computing environment

USAGE:
  renvctl COMMAND

COMMAND:
  help               Show this message
  check              Check whether all required pre-conditions (e.g dependencies) hold
  nix:install        Install nix
  nix:cache:install  Install binary cache
  nix:update         Update nix
  nix:prune          Prune old generations and collect relevant garbage
  home:install       Install home-manage
  home:apply         Install packages with home-manager
  ocaml:install      Install OCaml toolchains
  lean:install       Install Lean toolchains managed by Elan
  rust:install       Install Rust toolchains managed by Rustup
  vscode:generate    Re-generate VSCode market extensions manifest
```

## Post-Installation

### Install Essential Tools

Depending on the distro:

```bash
apt install build-essential make
```

### Set Fish as Default Shell

Append to shells and then set the shell:

```bash
echo /home/rvarago/.nix-profile/bin/fish >> "/etc/shells" && chsh -s /home/rvarago/.nix-profile/bin/fish $USER
```

### Setup Docker

Install docker by following the steps:

- <https://docs.docker.com/engine/install/>
