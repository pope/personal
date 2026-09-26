{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.photography;
  cpuArch = config.my.home.cpu.arch;

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

  darktablePkg = pkgs.darktable.overrideAttrs (
    oldAttrs:
    {
      postPatch = (oldAttrs.postPatch or "") + ''
        substituteInPlace data/noiseprofiles.json \
          --replace-fail '"model": "M11"' '"model": "M11-D"'
      '';
    }
    // (lib.optionalAttrs (cpuArch == "znver4") {
      CMAKE_C_FLAGS = "-march=znver4 -mtune=znver4";
      CMAKE_CXX_FLAGS = "-march=znver4 -mtune=znver4";
    })
  );
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
      darktablePkg
      digikamPkg
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
