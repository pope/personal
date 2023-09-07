# NixOS Flake for pope

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
