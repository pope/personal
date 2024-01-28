{ config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.desktop == "pantheon") {
    services.xserver = {
      desktopManager.pantheon.enable = true;
      displayManager.lightdm = {
        enable = true;
        greeters.pantheon.enable = true;
      };
    };
  };
}
