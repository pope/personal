{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.languages.zig;
in
{
  options.my.home.languages.zig = {
    enable = lib.mkEnableOption "Zig language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      zig
      zls
    ];
  };
}
