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
        obs-gstreamer
        obs-pipewire-audio-capture
        obs-vaapi
        obs-vkcapture
      ];
    };

    home.packages = with pkgs; [
      gphoto2
      gst_all_1.gst-plugins-bad
      gst_all_1.gst-plugins-base
      gst_all_1.gst-plugins-good
      gst_all_1.gst-plugins-ugly
      gst_all_1.gst-vaapi
      gst_all_1.gstreamer
    ];
  };
}
