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

  darktableBase = pkgs.darktable.overrideAttrs (
    oldAttrs:
    {
      nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.jq ];

      postPatch = (oldAttrs.postPatch or "") + ''
        jq '(.noiseprofiles[].models) |= . + [ .[] | select(.model == "M11") | .model = "M11-D" ]' \
          data/noiseprofiles.json > data/noiseprofiles.json.tmp
        mv data/noiseprofiles.json.tmp data/noiseprofiles.json
      '';
    }
    // (lib.optionalAttrs (cpuArch == "znver4") {
      CMAKE_C_FLAGS = "-march=znver4 -mtune=znver4";
      CMAKE_CXX_FLAGS = "-march=znver4 -mtune=znver4";
    })
  );

  darktablePkg = pkgs.symlinkJoin {
    name = "darktable-${darktableBase.version}";
    paths = [ darktableBase ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      for bin in darktable darktable-cli; do
        if [ -e "$out/bin/$bin" ]; then
          wrapProgram "$out/bin/$bin" \
            --add-flags "--conf plugins/ai/ort_library_path=${pkgs.onnxruntime}/lib/libonnxruntime.so"
        fi
      done

      for desktopFile in "$out/share/applications"/*.desktop; do
        if [ -f "$desktopFile" ]; then
          target=$(readlink -f "$desktopFile")
          rm "$desktopFile"
          sed \
            -e "s|${darktableBase}/bin/darktable|$out/bin/darktable|g" \
            "$target" > "$desktopFile"
        fi
      done
    '';
  };
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
      darktable-nightly
      digikamPkg
      dnglab
      geeqie
      rawtherapee
    ];
  };
}
