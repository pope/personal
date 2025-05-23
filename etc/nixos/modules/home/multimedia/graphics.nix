{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.multimedia.graphics;
in
{
  options.my.home.multimedia.graphics = {
    enable = mkEnableOption "Graphics and imaging multimedia home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs.stable; [
      aseprite
      (pkgs.gimp3-with-plugins.override {
        plugins = with pkgs.gimpPlugins; [
          bimp
          gimplensfun
          gmic
          lqrPlugin
          waveletSharpen
        ];
      })
      inkscape
      krita
    ];
  };
}
