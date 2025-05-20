{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkOption mkEnableOption types;
  cfg = config.my.home.terminals.foot;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.terminals.foot = {
    enable = mkEnableOption "Foot terminal home options";
    fontSize = mkOption {
      type = types.number;
      default = 12;
      description = lib.mkDoc ''
        The font size to use
      '';
    };
  };

  config = mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          include =
            if colorScheme == "rose-pine" then "${pkgs.foot.themes}/share/foot/themes/rose-pine"
            else if colorScheme == "catppuccin" then "${pkgs.foot.themes}/share/foot/themes/catppuccin-mocha"
            else if colorScheme == "dracula" then "${pkgs.foot.themes}/share/foot/themes/dracula"
            else if colorScheme == "tokyonight" then "${pkgs.foot.themes}/share/foot/themes/tokyonight-storm"
            else abort "invalid theme";
          box-drawings-uses-font-glyphs = true;
          dpi-aware = false;
          font =
            let
              fontSize = builtins.toString cfg.fontSize;
            in
            "monospace:size=${fontSize}, JetBrainsMono Nerd Font:size=${fontSize}";
          font-size-adjustment = 0.5;
          line-height = cfg.fontSize * 1.25;
          pad = "4x10";
        };
        colors = {
          alpha = 0.96;
        };
        cursor = {
          style = "beam";
          blink = true;
        };
        mouse = {
          hide-when-typing = true;
        };
      };
    };
  };
}
