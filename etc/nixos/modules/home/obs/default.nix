{ pkgs, config, lib, ... }:

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
        input-overlay
        obs-ndi
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
