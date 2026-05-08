{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.gaming;
in
{
  options.my.home.gaming = {
    enable = lib.mkEnableOption "Gaming home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      goverlay
      lutris
      mangohud
      vkbasalt
    ];
  };
}
