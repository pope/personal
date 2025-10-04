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
    hip.enable = lib.mkEnableOption "enable AMD HIP";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs.stable; [
      (if cfg.hip.enable then blender-hip else blender)
    ];
  };
}
