{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.home.gtk;

  package = pkgs.tokyonight-gtk-theme.override {
    sizeVariants = [
      "standard"
      "compact"
    ];
    themeVariants = [ "all" ];
    tweakVariants = [ "moon" ];
    iconVariants = [ "Moon" ];
  };
in
{
  config = lib.mkIf (cfg.enable && cfg.theme == "tokyonight") {
    gtk = {
      iconTheme = {
        inherit package;
        name = "Tokyonight-Moon";
      };

      theme = {
        inherit package;
        name = "Tokyonight-Purple-Dark-Moon";
      };
    };

    home.pointerCursor = {
      name = "Bibata-Modern-Ice";
      size = 24;
      package = pkgs.bibata-cursors;
    };

    my.home.gtk.darkTheme = true;
  };
}
