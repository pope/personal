{ config, pkgs, lib, ... }:

let
  cfg = config.my.home.terminals.ghostty;

  inherit (config.my.home.theme) colorScheme;
in
{
  options.my.home.terminals.ghostty = {
    enable = lib.mkEnableOption "Ghostty terminal home options";
    fontSize = lib.mkOption {
      type = lib.types.number;
      default = 12;
      description = lib.mkDoc ''
        The font size to use
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      package =
        if pkgs.stdenv.isDarwin then pkgs.bashInteractive
        else config.lib.nixGL.wrap pkgs.ghostty;
      enableFishIntegration = config.my.home.shell.fish.enable;
      enableZshIntegration = config.my.home.shell.zsh.enable;
      installBatSyntax = !pkgs.stdenv.isDarwin;
      settings =
        let
          theme =
            if colorScheme == "rose-pine" then "Rose Pine"
            else if colorScheme == "catppuccin" then "Catppuccin Mocha"
            else if colorScheme == "dracula" then "Dracula"
            else if colorScheme == "tokyonight" then "TokyoNight"
            else abort "invalid colorScheme";
        in
        {
          inherit theme;
          adjust-cell-height = "40%";
          gtk-toolbar-style = "flat";
          background-opacity = 0.95;
          background-blur-radius = 20;
          config-file = [ "?overrides" ];
          font-family = [
            ""
            "monospace"
          ];
          font-size = cfg.fontSize;
          window-decoration = "auto";
          window-theme = "ghostty";
        };
    };
  };
}
