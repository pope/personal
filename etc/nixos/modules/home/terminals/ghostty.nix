{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.terminals.ghostty;

  configDir =
    if pkgs.stdenv.isDarwin then
      "Library/Application Support/com.mitchellh.ghostty"
    else
      "${config.xdg.configHome}/ghostty";

  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.terminals.ghostty = {
    enable = mkEnableOption "Ghostty terminal home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      ghostty
    ];

    home.file."${configDir}/config".text =
      let
        theme =
          if colorScheme == "rose-pine" then "rose-pine"
          else if colorScheme == "catppuccin" then "catppuccin-mocha"
          else if colorScheme == "dracula" then "Dracula"
          else if colorScheme == "tokyonight" then "tokyonight"
          else abort "invalid colorScheme";
      in
      ''
        adjust-cell-height = "25%"

        background-opacity = 0.95

        font-family = ""
        font-family = "Liga SFMono Nerd Font"
        font-family = "MonoLisa"
        font-size = 9

        theme = "${theme}"

        window-decoration = false
      '';
  };
}
