{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.keymapp;
in
{
  options.my.home.keymapp = {
    enable = lib.mkEnableOption "Keymapp home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      keymapp
    ];
  };
}
