{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.home.terminals.foot;
  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.terminals.foot = {
    enable = lib.mkEnableOption "Foot terminal home options";
    fontSize = lib.mkOption {
      type = lib.types.number;
      default = 12;
      description = lib.mkDoc ''
        The font size to use
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    programs.foot = {
      enable = true;
      settings = {
        main = {
          include =
            let
              cs =
                if colorScheme == "rose-pine" then
                  "rose-pine"
                else if colorScheme == "catppuccin" then
                  "catppuccin-mocha"
                else if colorScheme == "dracula" then
                  "dracula"
                else if colorScheme == "tokyonight" then
                  "tokyonight-storm"
                else
                  abort "invalid theme";
            in
            [ "${pkgs.foot.themes}/share/foot/themes/${cs}" ];
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
