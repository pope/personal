{ config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.desktop == "kde") {
    services.xserver = {
      desktopManager.plasma6.enable = true;
      displayManager.sddm.enable = true;
    };
  };
}
