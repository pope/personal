{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.gtk;
in
{
  config = lib.mkIf (cfg.enable && cfg.theme == "rose-pine") rec {
    gtk = {
      iconTheme = {
        name = "rose-pine";
        package = pkgs.rose-pine-icon-theme;
      };

      theme = {
        name = "rose-pine";
        package = pkgs.rose-pine-gtk-theme;
      };
    };

    home.pointerCursor = {
      name = "BreezeX-RosePine-Linux";
      size = 24;
      package = pkgs.rose-pine-cursor;
    };

    my.home.gtk.darkTheme = false;

    xdg =
      let
        themeDir = "${gtk.theme.package}/share/themes/${gtk.theme.name}";
      in
      {
        configFile."gtk-4.0/gtk.css".source = "${themeDir}/gtk-4.0/gtk.css";
      };
  };
}
