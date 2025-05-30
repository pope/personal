{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.gtk;
  inherit (config.my.home.theme) colorScheme;
in
{
  config = lib.mkIf (cfg.enable && colorScheme == "catppuccin") rec {
    home.pointerCursor = {
      name = "macOS-White";
      size = 24;
      package = pkgs.apple-cursor;
      gtk.enable = true;
      x11.enable = true;
    };

    gtk = {
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.catppuccin-papirus-folders.override {
          accent = "mauve";
          flavor = "mocha";
        };
      };

      theme = {
        name = "Catppuccin-GTK-Purple-Dark-Compact";
        package = pkgs.magnetic-catppuccin-gtk.override {
          accent = [ "purple" ];
          shade = "dark";
          size = "compact";
        };
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
