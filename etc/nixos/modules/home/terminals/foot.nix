{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption mkEnableOption types;
  cfg = config.my.home.terminals.foot;
in
{
  options.my.home.terminals.foot = {
    enable = mkEnableOption "Foot terminal home options";
    colorScheme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which color theme to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          include =
            if cfg.colorScheme == "rose-pine" then "${pkgs.foot.themes}/share/foot/themes/rose-pine"
            else if cfg.colorScheme == "catppuccin" then "${pkgs.foot.themes}/share/foot/themes/catppuccin-mocha"
            else if cfg.colorScheme == "dracula" then "${pkgs.foot.themes}/share/foot/themes/dracula"
            else if cfg.colorScheme == "tokyonight" then "${pkgs.foot.themes}/share/foot/themes/tokyonight-storm"
            else abort "invalid theme";
          dpi-aware = false;
          font = "monospace:size=12, Symbols Nerd Font:size=12";
        };
        colors = {
          alpha = 0.96;
        };
      };
    };
  };
}
