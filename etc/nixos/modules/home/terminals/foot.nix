{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.terminals.foot;
in
{
  options.my.home.terminals.foot = {
    enable = mkEnableOption "Foot terminal home options";
  };

  config = mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          dpi-aware = true;
          font = "Iosevka Comfy:size=10, Symbols Nerd Font:size=10";
          # TODO(pope): Allow for other themes
          include = "${lib.getOutput "themes" pkgs.foot}/share/foot/themes/rose-pine";
        };
      };
    };
  };
}

