{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  inherit (config.lib.formats.rasi) mkLiteral;
  cfg = config.my.home.rofi;
in
{
  options.my.home.rofi = {
    enable = mkEnableOption "rofi home options";
  };

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      plugins = with pkgs; [
        rofi-emoji-wayland
        rofi-calc
      ];
      font = "mono 10";
      extraConfig = {
        display-drun = "🔍 ";
        display-run = "🏃 ";
        display-emoji = "🤓 ";
        display-calc = "🧮 ";
        modes = [ "drun" "emoji" "calc" "run" ];
        show-icons = true;
      };
      theme = with config.my.home.theme.colors.withHash;
        let
          transparency = if config.my.home.hyprland.enable then "B2" else "F2";
          fixedHeight = lib.boolToString config.my.home.hyprland.enable;
        in
        {
          "*" = {
            bg0 = mkLiteral "${base00}${transparency}";
            bg1 = mkLiteral base00;
            bg2 = mkLiteral "${base02}7F";
            bg3 = mkLiteral "${base0E}7F";

            fg0 = mkLiteral base05;
            fg1 = mkLiteral "#FFFFFF";
            fg2 = mkLiteral base04;
            fg3 = mkLiteral base04;

            fixedHeight = mkLiteral fixedHeight;
          };

          "@import" = "${./rounded-common.rasi}";
        };
    };
  };
}
