{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.gaming;
in
{
  options.my.home.gaming = {
    enable = mkEnableOption "Gaming home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      goverlay
      lutris
      mangohud
      vkbasalt
    ];
  };
}
