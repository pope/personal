{ config, lib, ... }:

let
  cfg = config.my.nixos.virtualization;
in
{
  config = lib.mkIf (cfg.enable && cfg.kind == "guest") {
    services = {
      qemuGuest.enable = true;
      spice-vdagentd.enable = true;
    };
    virtualisation.vmVariant = {
      virtualisation = {
        cores = 4;
        memorySize = 1024 * 4;
        qemu.options = [
          "-device virtio-vga-gl"
          "-display gtk,show-menubar=on,zoom-to-fit=off,gl=on"
        ];
      };
    };
  };
}
