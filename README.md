# renv

> "Rafael's Environment", a bunch of quick & dirty, perhaps awkward hacks tailored for my own usage.

My computing environment based on [nix](https://github.com/NixOS/nix)/[home-manager](https://github.com/nix-community/home-manager).

## Introduction

The script `renvctl` orchestrates part of the process of installing components with `home-manager` and similar:

```sh
./renvctl help
```

## Installation

> IMPORTANT: Ensure that [settings.nix](./nixpkgs/settings.nix) is correct for the environment (e.g. username matches the system, email)

Install system components:
```sh
./renvctl system:install1
```

After rebooting, complete the installation:
```sh
./renvctl system:install2
```

## Usage

Upgrade everything (except home):
```sh
./renvctl system:sync
```

Hack around and sync changes to home:
```sh
./renvctl home:sync
```

Install Visual Studio Code:
```sh
./renvctl vscode:sync
```

Install a language toolchain:
```sh
./renvctl $LANG:install
```
Where `$LANG in {ocaml, lean, rust}`

## Post-Installation

### Setup Docker

Install docker by following the steps:

- <https://docs.docker.com/engine/install/>

## TODOs
