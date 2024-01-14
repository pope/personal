{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.gaming;
in
{
  options.my.nixos.gaming = {
    enable = mkEnableOption "gaming system options";
  };

  config = mkIf cfg.enable {
    hardware = {
      steam-hardware.enable = true;
    };
  };
}
