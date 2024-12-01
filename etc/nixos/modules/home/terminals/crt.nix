{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.terminals.crt;
in
{
  options.my.home.terminals.crt = {
    enable = mkEnableOption "Cool Retro Term terminal home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      cool-retro-term
      nerd-fonts.terminess-ttf
    ];
  };
}
