{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.home.gtk;
in
{
  config = mkIf (cfg.enable && cfg.theme == "catppuccin") rec {
    gtk = {
      iconTheme = {
        name = "Papirus-Dark";
        package = pkgs.catppuccin-papirus-folders.override {
          accent = "mauve";
          flavor = "mocha";
        };
      };

      theme = {
        name = "Catppuccin-Mocha-Standard-Mauve-dark";
        package = pkgs.catppuccin-gtk.override {
          size = "standard";
          accents = [ "mauve" ];
          variant = "mocha";
          tweaks = [ "normal" ];
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

    xdg =
      let
        themeDir = "${gtk.theme.package}/share/themes/${gtk.theme.name}";
      in
      {
        configFile."gtk-4.0/assets" = {
          source = "${themeDir}/gtk-4.0/assets";
          recursive = true;
        };
        configFile."gtk-4.0/gtk.css".source = "${themeDir}/gtk-4.0/gtk.css";
        configFile."gtk-4.0/gtk-dark.css".source = "${themeDir}/gtk-4.0/gtk-dark.css";
      };
  };
}
