{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.graphics;
  cpuArch = config.my.home.cpu.arch;
in
{
  options.my.home.multimedia.graphics = {
    enable = lib.mkEnableOption "Graphics and imaging multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        aseprite
        inkscape
        jxrlib
        krita
        synfigstudio
      ]
      ++ (lib.optionals (cpuArch == "unspecified") [
        pkgs.gimp-with-plugins
      ])
      ++ (lib.optionals (cpuArch == "znver4") [
        (pkgs.gimp-with-plugins.override {
          gimpPlugins = pkgs.gimpPlugins // {
            gimp = pkgs.gimpPlugins.gimp.override { inherit (pkgs.znver4) stdenv; };
            gmic = pkgs.gimpPlugins.gmic.override { inherit (pkgs.znver4) stdenv; };
          };
        })
      ]);
  };
}
