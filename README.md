# renv

> Rafael's Environment.

My computing environment.

# Post Installation

# Add Desktop Entries

Append this to `.profile`:

```bash
if [ -e /home/rvarago/.nix-profile/etc/profile.d/nix.sh ]; then
    XDG_DATA_DIRS="/home/rvarago/.nix-profile/share:$XDG_DATA_DIRS"
fi
```

# Set Fish as Default Shell

Append this to `/etc/shells`:

```
/home/rvarago/.nix-profile/bin/fish
```

Run:

```
chsh -s /home/rvarago/.nix-profile/bin/fis
```
