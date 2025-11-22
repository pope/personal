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
  config = lib.mkIf (cfg.enable && cfg.theme == "dracula") {
    gtk = {
      iconTheme = {
        name = "Dracula";
        package = pkgs.dracula-icon-theme;
      };

      theme = {
        name = "Dracula";
        package = pkgs.dracula-theme;
      };

    };

    home.pointerCursor = {
      name = "catppuccin-mocha-mauve-cursors";
      size = 32;
      package = pkgs.catppuccin-cursors.mochaMauve;
    };

    my.home.gtk.darkTheme = true;
  };
}
