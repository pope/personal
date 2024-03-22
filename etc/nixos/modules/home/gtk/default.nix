{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.home.gtk;
in
{
  imports = [
    ./catppuccin.nix
    ./rose-pine.nix
  ];

  options.my.home.gtk = {
    enable = mkEnableOption "GTK home options";
    theme = mkOption {
      type = types.enum [ "rose-pine" "catppuccin" ];
      default = "rose-pine";
      description = lib.mkDoc ''
        Which theme to use with the GTK configuration.
      '';
    };
  };

  config = mkIf cfg.enable rec {
    home.pointerCursor = {
      name = "Catppuccin-Mocha-Mauve-Cursors";
      size = 32;
      package = pkgs.catppuccin-cursors.mochaMauve;
      gtk.enable = true;
      x11.enable = true;
    };

    gtk = {
      enable = true;

      font = {
        name = "Helvetica Neue LT Std, Light";
        # name = "Work Sans, Thin";
        package = pkgs.helvetica-neue-lt-std;
        size = 11;
      };

      cursorTheme = {
        inherit (home.pointerCursor) name size package;
      };

      gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
      gtk2.extraConfig = ''
        gtk-xft-antialias=1
        gtk-xft-hinting=1
        gtk-xft-hintstyle="hintslight"
        gtk-xft-rgba="rgb"
      '';

      gtk3.extraConfig = {
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintslight";
        gtk-xft-rgba = "rgb";
      };
    };

    # When this is enabled, KDE and Plasma don't work for Wayland.
    # But this is the style I like for non-KDE stuff, so will re-work later.
    qt = {
      enable = true;
      platformTheme = "gtk3";
    };

    services.xsettingsd = {
      enable = true;
      settings = {
        "Gtk/CursorThemeName" = "${gtk.cursorTheme.name}";
      };
    };
  };
}
