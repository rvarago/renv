# renv

> Rafael's Environment.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Usage

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
λ   ./renvctl -h
Manage my computing environment

USAGE:
  renvctl [OPTION] COMMAND

OPTION:
  -h                      Show this message

COMMAND:
  check-deps              Check whether all required pre-conditions (e.g dependencies) hold
  home                    Install packages with home-manager
  lean                    Install Lean toolchains managed by Elan (imperative)
  home-manager            Install home-manage (imperative)
  nix                     Install nix (imperative)
  nix-update              Update nix (imperative)
  ocaml                   Install OCaml toolchain (imperative)
  prune                   Prune old generations and collect relevant garbage
  rust                    Install Rust toolchains managed by Rustup (imperative)
  vscode                  Re-generate VSCode market extensions manifest
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

### Configure Wireshark

Create the `wireshark` group and add my user to it.

```sh
groupadd wireshark && usermod -aG wireshark $USER
```
