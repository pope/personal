{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.obs;
in
{
  options.my.home.obs = {
    enable = lib.mkEnableOption "OBS Studio home options";
  };

  config = lib.mkIf cfg.enable {
    programs.obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        # TODO(pope): Remove this override after the NDI updater script runs
        (distroav.override {
          ndi-6 = pkgs.ndi-6.overrideAttrs (_: {
            src = pkgs.fetchurl {
              url = "https://downloads.ndi.tv/SDK/NDI_SDK_Linux/Install_NDI_SDK_v6_Linux.tar.gz";
              hash = "sha256-8DFPJFRG3vxIi2POtGiazxqWWu79ray3BXG7IWqMwYM=";
            };
          });
        })
        input-overlay
        obs-pipewire-audio-capture
        obs-vaapi
        obs-vkcapture
      ];
    };

    home = {
      packages = with pkgs; [
        gphoto2
      ];
      sessionVariables = {
        "OBS_VKCAPTURE" = "1";
      };
    };
  };
}
