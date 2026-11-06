{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.photography;

  digikamPkg =
    if cfg.opencl.enable then
      pkgs.symlinkJoin {
        name = "digikam-${pkgs.digikam.version}";
        paths = [ pkgs.digikam ];
        buildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          for bin in digikam showfoto; do
            if [ -e "$out/bin/$bin" ]; then
              wrapProgram "$out/bin/$bin" \
                --set OPENCV_OPENCL_DEVICE "${cfg.opencl.device}"
            fi
          done
        '';
      }
    else
      pkgs.digikam;
in
{
  options.my.home.multimedia.photography = {
    enable = lib.mkEnableOption "Photography multimedia home options";
    opencl = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = pkgs.config.rocmSupport or false;
        description = "Enable OpenCL GPU acceleration for digiKam";
      };
      device = lib.mkOption {
        type = lib.types.str;
        default = ":GPU:0";
        description = "OpenCV OpenCL target device string";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      darktable
      darktable-nightly
      digikamPkg
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
