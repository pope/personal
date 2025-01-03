{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
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
    fontSize = mkOption {
      type = types.number;
      default = 12;
      description = lib.mkDoc ''
        The font size to use
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = lib.optionals pkgs.stdenv.isLinux (with pkgs; [
      ghostty
    ]);

    home.file."${configDir}/config".text =
      let
        theme =
          if colorScheme == "rose-pine" then "rose-pine"
          else if colorScheme == "catppuccin" then "catppuccin-mocha"
          else if colorScheme == "dracula" then "Dracula"
          else if colorScheme == "tokyonight" then "tokyonight"
          else abort "invalid colorScheme";
        window-decoration = if pkgs.stdenv.isLinux then "false" else "true";
      in
      ''
        adjust-cell-height = "25%"

        background-opacity = 0.95
        background-blur-radius = 20

        font-family = ""
        font-family = "Liga SFMono Nerd Font"
        font-family = "MonoLisa"
        font-size = ${builtins.toString cfg.fontSize}

        theme = "${theme}"

        window-decoration = ${window-decoration}

        config-file = ?overrides
      '';
  };
}
