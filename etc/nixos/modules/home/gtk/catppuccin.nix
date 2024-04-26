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
        name = "Catppuccin-Mocha-Standard-Mauve-Dark";
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
  };
}
