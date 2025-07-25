{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.multimedia.photography;
in
{
  options.my.home.multimedia.photography = {
    enable = lib.mkEnableOption "Photography multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      darktable
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
