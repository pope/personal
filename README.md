# NixOS Flake for pope

## Structure

### `modules/nixos/`

These are NixOS specific modules. Including it sets up a main user and can include subsets of common things I use with a `my.nixos.system.<module>.enable`. Some modules are also configurable.

### `modules/home/`

These are home-manager modules.

### `hosts/`

These are the files for each host. If the host runs on NixOS, then the `default.nix` file will configure the system. For home-manager files, those will be loaded by the `home.nix` file.

### `overlays/`

These are Nix overlays which can be loaded up and used.

## SOPS Nix

In order to use the SOPS nix secrets, the key needs to be stored in `~/.config/sops/age/keys.txt`. It can be extracted from 1Password.

```sh
mkdir -p ~/.config/sops/age
op read "op://NixOS/Sops Age Keys/keys.txt" -o ~/.config/sops/age/keys.txt
```

## Remote builders

It's possible to use another machine to do building when trying to update the system.

```sh
sudo nixos-rebuild --build-host pope@soundwave.lan switch
```
