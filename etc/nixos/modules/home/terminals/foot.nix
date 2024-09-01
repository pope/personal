{ config, lib, ... }:

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
      settings = with config.my.home.theme.colors; {
        main = {
          dpi-aware = true;
          font = "Iosevka Comfy:size=10, Symbols Nerd Font:size=10";
        };
        cursor = {
          color = "${base00} ${base05}";
        };
        colors = {
          background = "${base00}";
          foreground = "${base05}";

          regular0 = "${base02}"; # black
          regular1 = "${base08}"; # red
          regular2 = "${base0C}"; # green
          regular3 = "${base09}"; # yellow
          regular4 = "${base0B}"; # blue
          regular5 = "${base0D}"; # magenta
          regular6 = "${base0A}"; # cyan
          regular7 = "${base05}"; # white

          bright0 = "${base02}"; # bright black
          bright1 = "${base08}"; # bright red
          bright2 = "${base0C}"; # bright green
          bright3 = "${base09}"; # bright yellow
          bright4 = "${base0B}"; # bright blue
          bright5 = "${base0D}"; # bright magenta
          bright6 = "${base0A}"; # bright cyan
          bright7 = "${base05}"; # bright white
        };
      };
    };
  };
}

