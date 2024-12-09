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
          dpi-aware = false;
          font = "monospace:size=12, Symbols Nerd Font:size=12";
        };
        cursor = {
          color = "${base00} ${base05}";
        };
        colors = {
          alpha = 0.96;
          background = "${base00}";
          foreground = "${base05}";

          regular0 = "${base03}"; # black
          regular1 = "${base08}"; # red
          regular2 = "${base0B}"; # green
          regular3 = "${base0A}"; # yellow
          regular4 = "${base0D}"; # blue
          regular5 = "${base0E}"; # magenta
          regular6 = "${base0C}"; # cyan
          regular7 = "${base05}"; # white

          bright0 = "${base04}"; # bright black
          bright1 = "${base08}"; # bright red
          bright2 = "${base0B}"; # bright green
          bright3 = "${base0A}"; # bright yellow
          bright4 = "${base0D}"; # bright blue
          bright5 = "${base0E}"; # bright magenta
          bright6 = "${base0C}"; # bright cyan
          bright7 = "ffffff"; # bright white

          selection-foreground = "${base05}";
          selection-background = "${base03}";

          search-box-no-match = "${base01} ${base08}";
          search-box-match = "${base05} ${base02}";

          jump-labels = "${base02} ${base09}";
          urls = "${base0D}";
        };
      };
    };
  };
}
