{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.languages.go;
in
{
  options.my.home.languages.go = {
    enable = lib.mkEnableOption "Go language home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      go
      gopls
    ];
  };
}
