{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.photography;
in
{
  options.my.home.multimedia.photography = {
    enable = mkEnableOption "Photography multimedia home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      darktable
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
