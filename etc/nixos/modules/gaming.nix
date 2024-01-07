{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.system.gaming;
in
{
  options.my.system.gaming = {
    enable = mkEnableOption "gaming system options";
  };

  config = mkIf cfg.enable {
    hardware = {
      steam-hardware.enable = true;
    };
  };
}
