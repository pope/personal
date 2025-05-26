{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.terminals.ghostty;

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
    programs.ghostty = {
      enable = true;
      package = if pkgs.stdenv.isDarwin then pkgs.bashInteractive else pkgs.ghostty;
      enableFishIntegration = config.my.home.shell.fish.enable;
      enableZshIntegration = config.my.home.shell.zsh.enable;
      installBatSyntax = !pkgs.stdenv.isDarwin;
      settings =
        let
          theme =
            if colorScheme == "rose-pine" then "rose-pine"
            else if colorScheme == "catppuccin" then "catppuccin-mocha"
            else if colorScheme == "dracula" then "Dracula"
            else if colorScheme == "tokyonight" then "tokyonight"
            else abort "invalid colorScheme";
        in
        {
          inherit theme;
          adjust-cell-height = "40%";
          adw-toolbar-style = "flat";
          background-opacity = 0.95;
          background-blur-radius = 20;
          config-file = [ "?overrides" ];
          font-family = [
            ""
            "monospace"
          ];
          font-size = cfg.fontSize;
          gtk-adwaita = true;
          gtk-single-instance = true; # default is "desktop"
          window-decoration = "auto";
          window-theme = "ghostty";
        };
    };
  };
}
