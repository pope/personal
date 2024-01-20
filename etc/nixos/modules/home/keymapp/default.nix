{ pkgs, lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.keymapp;
in
{
  options.my.home.keymapp = {
    enable = mkEnableOption "Keymapp home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      keymapp
    ];
  };
}
