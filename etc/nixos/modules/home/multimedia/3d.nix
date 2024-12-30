{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.threed;
in
{
  options.my.home.multimedia.threed = {
    enable = mkEnableOption "3d multimedia home options";
    hip.enable = mkEnableOption "enable AMD HIP";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.stable; [
      (if cfg.hip.enable then blender-hip else blender)
    ];
  };
}
