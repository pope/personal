{ config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.virtualization;
in
{
  config = mkIf (cfg.enable && cfg.kind == "guest") {
    services = {
      qemuGuest.enable = true;
      spice-vdagentd.enable = true;
    };
  };
}
