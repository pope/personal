{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.gtk;
in
{
  config = lib.mkIf (cfg.enable && cfg.theme == "catppuccin") {
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
    };

    home.pointerCursor = {
      name = "macOS-White";
      size = 24;
      package = pkgs.apple-cursor;
    };

    my.home.gtk.darkTheme = true;
  };
}
