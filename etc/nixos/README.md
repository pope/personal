# NixOS Flake for pope

## Structure

### `modules/nixos/`

These are NixOS specific modules. Including it sets up a main user and can
include subsets of common things I use with a
`my.nixos.system.<module>.enable`. Some modules are also configurable.

### `home/`

These are home-manager modules. Right now, each home module must be imported.
But soon it will behave just like `modules/nixos`.

### `hosts/`

These are the files for each host. If the host runs on NixOS, then the
`default.nix` file will configure the system. For home-manager files, those
will be loaded by the `home.nix` file.

### `overlays/`

These are Nix overlays which can be loaded up and used.

## Raspberry Pi

How to build the SD card:

  1. `nix build .#nixosConfigurations.raspberrypi.config.formats.sd-aarch64`
  2. `nix-shell -p zstd`
  3. `zstd -d $(readlink -f result) -c | sudo dd of=/dev/sdg bs=4096 conv=fsync status=progress`

Where `/dev/sdg` is the location of the SD card. And the command above assumes
Bash shell.

After the server starts, make sure change the password and to add a Samba user:

```sh
sudo smbpasswd -a pi
```

### Remote deployment

It's also easy enough to build on a local machine the Pi dependencies and then
copy those files over to the Pi for quick updates. The local machine will
need `boot.binfmt.emulatedSystems = [ "aarch64-linux" ];` set up in order to
be able to build for the ARM architecture.

```sh
nixos-rebuild --flake .#raspberrypi --target-host pi@raspberrypi.lan --use-remote-sudo switch
```

## Remote builders

It's possible to use another machine to do building when trying to update the
system. 

```sh
sudo nixos-rebuild --build-host pope@soundwave.lan switch
```
