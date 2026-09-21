{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.graphics;
in
{
  options.my.home.multimedia.graphics = {
    enable = lib.mkEnableOption "Graphics and imaging multimedia home options";
    opencl.enable = lib.mkOption {
      type = lib.types.bool;
      default = pkgs.config.rocmSupport or false;
      description = "Enable OpenCL acceleration for GEGL/GIMP";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      aseprite
      gimp-with-plugins
      inkscape
      jxrlib
      krita
      pixieditor
      synfigstudio
    ];

    home.sessionVariables = lib.mkIf cfg.opencl.enable {
      GEGL_USE_OPENCL = "1";
    };
  };
}
