{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.gtk;
in
{
  imports = [
    ./catppuccin.nix
    ./dracula.nix
    ./rose-pine.nix
    ./tokyonight.nix
  ];

  options.my.home.gtk = {
    enable = lib.mkEnableOption "GTK home options";
  };

  config = lib.mkIf cfg.enable rec {
    gtk = {
      enable = true;

      font = {
        name = "SF Pro Display";
        package = pkgs.sf-pro;
        size = 11;
      };

      cursorTheme = {
        inherit (config.home.pointerCursor) name size package;
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
      platformTheme.name = "gtk3";
    };

    services.xsettingsd = {
      enable = true;
      settings = {
        "Gtk/CursorThemeName" = "${gtk.cursorTheme.name}";
      };
    };
  };
}
