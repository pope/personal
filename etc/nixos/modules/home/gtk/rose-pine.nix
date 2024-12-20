{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.home.gtk;
  inherit (config.my.home.theme) colorScheme;
in
{
  config = mkIf (cfg.enable && colorScheme == "rose-pine") rec {
    home.pointerCursor = {
      name = "BreezeX-RosePine-Linux";
      size = 24;
      package = pkgs.rose-pine-cursor;
      gtk.enable = true;
      x11.enable = true;
    };

    gtk = {
      iconTheme = {
        name = "rose-pine";
        package = pkgs.rose-pine-icon-theme;
      };

      theme = {
        name = "rose-pine";
        package = pkgs.rose-pine-gtk-theme;
      };

      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = 0;
      };

      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = 0;
      };
    };

    services.xsettingsd = {
      settings = {
        "Net/IconThemeName" = "${gtk.iconTheme.name}";
        "Net/ThemeName" = "${gtk.theme.name}";
      };
    };

    xdg =
      let
        themeDir = "${gtk.theme.package}/share/themes/${gtk.theme.name}";
      in
      {
        configFile."gtk-4.0/gtk.css".source = "${themeDir}/gtk-4.0/gtk.css";
      };
  };
}
