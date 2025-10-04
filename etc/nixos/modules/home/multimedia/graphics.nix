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
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs.stable; [
      aseprite
      (gimp3-with-plugins.override {
        plugins = with gimpPlugins; [
          bimp
          gimplensfun
          gmic
          lqrPlugin
          waveletSharpen
        ];
      })
      inkscape
      krita
      synfigstudio
    ];
  };
}
