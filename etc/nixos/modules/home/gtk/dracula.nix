{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.gtk;
  inherit (config.my.home.theme) colorScheme;
in
{
  config = lib.mkIf (cfg.enable && colorScheme == "dracula") rec {
    home.pointerCursor = {
      name = "catppuccin-mocha-mauve-cursors";
      size = 32;
      package = pkgs.catppuccin-cursors.mochaMauve;
      gtk.enable = true;
      x11.enable = true;
    };

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
