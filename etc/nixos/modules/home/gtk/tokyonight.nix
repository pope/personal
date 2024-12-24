{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.home.gtk;
  inherit (config.my.home.theme) colorScheme;

  package = pkgs.tokyonight-gtk-theme.override {
    sizeVariants = [ "standard" "compact" ];
    themeVariants = [ "all" ];
    tweakVariants = [ "moon" ];
    iconVariants = [ "Moon" ];
  };
in
{
  config = mkIf (cfg.enable && colorScheme == "tokyonight") rec {
    home.pointerCursor = {
      name = "Bibata-Modern-Ice";
      size = 24;
      package = pkgs.bibata-cursors;
      gtk.enable = true;
      x11.enable = true;
    };

    gtk = {
      iconTheme = {
        inherit package;
        name = "Tokyonight-Moon";
      };

      theme = {
        inherit package;
        name = "Tokyonight-Purple-Dark-Moon";
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

