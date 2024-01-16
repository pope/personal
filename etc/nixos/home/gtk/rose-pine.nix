{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.home.gtk;
in
{
  config = mkIf (cfg.enable && cfg.theme == "rose-pine") rec {
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
