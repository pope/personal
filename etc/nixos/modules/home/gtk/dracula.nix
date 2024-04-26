{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.home.gtk;
in
{
  config = mkIf (cfg.enable && cfg.theme == "dracula") rec {
    gtk = {
      iconTheme = {
        name = "Dracula";
        package = pkgs.dracula-icon-theme;
      };

      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };

      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = 1;
      };

      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = 1;
      };
    };

    services.xsettingsd = {
      settings = {
        "Net/IconThemeName" = "${gtk.iconTheme.name}";
        "Net/ThemeName" = "${gtk.theme.name}";
      };
    };
  };
}
