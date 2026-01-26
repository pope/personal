{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.multimedia.photography;
  cpuArch = config.my.home.cpu.arch;
in
{
  options.my.home.multimedia.photography = {
    enable = lib.mkEnableOption "Photography multimedia home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      with pkgs;
      [
        digikam
        dnglab
        geeqie
        rawtherapee
      ]
      ++ (lib.optionals (cpuArch == "unspecified") [
        unstable.darktable
      ])
      ++ (lib.optionals (cpuArch == "znver4") [
        (unstable.darktable.override { inherit (znver4) stdenv; })
      ]);
  };
}
