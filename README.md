# renv

> Rafael's Environment.

My computing environment.

# Post-Installation

## Add Desktop Entries

Append this to `.profile`:

```bash
if [ -e /home/rvarago/.nix-profile/etc/profile.d/nix.sh ]; then
    XDG_DATA_DIRS="${XDG_DATA_DIRS}:/home/rvarago/.nix-profile/share"
fi
```

## Set Fish as Default Shell

Append to shells and then set the shell:

```bash
echo /home/rvarago/.nix-profile/bin/fish >> "/etc/shells" && chsh -s /home/rvarago/.nix-profile/bin/fish
```
