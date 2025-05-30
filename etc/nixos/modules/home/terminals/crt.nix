{ config, pkgs, lib, ... }:

let
  cfg = config.my.home.terminals.crt;
in
{
  options.my.home.terminals.crt = {
    enable = lib.mkEnableOption "Cool Retro Term terminal home options";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [
      cool-retro-term
      nerd-fonts.terminess-ttf
    ];
  };
}
