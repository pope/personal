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
  config = lib.mkIf (cfg.enable && cfg.theme == "breeze") {
    gtk = {
      iconTheme = {
        name = "breeze-dark";
        package = pkgs.kdePackages.breeze-icons;
      };

      theme = {
        name = "Breeze Dark";
        package = pkgs.kdePackages.breeze-gtk;
      };
    };

    home.pointerCursor = {
      name = "breeze_cursors";
      size = 24;
      package = pkgs.kdePackages.breeze;
    };

    my.home.gtk.darkTheme = true;
  };
}
