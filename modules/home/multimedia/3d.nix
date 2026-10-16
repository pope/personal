{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.threed;
in
{
  options.my.home.multimedia.threed = {
    enable = lib.mkEnableOption "3d multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      blender
    ];
  };
}
