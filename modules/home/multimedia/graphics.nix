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
    home.packages = with pkgs; [
      aseprite
      gimp-with-plugins
      inkscape
      jxrlib
      krita
      pixieditor
      synfigstudio
    ];
  };
}
