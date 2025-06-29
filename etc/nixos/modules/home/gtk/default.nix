{ pkgs, config, lib, ... }:

let
  cfg = config.my.home.gtk;
in
{
  imports = [
    ./breeze.nix
    ./catppuccin.nix
    ./dracula.nix
    ./rose-pine.nix
    ./tokyonight.nix
  ];

  options.my.home.gtk = {
    enable = lib.mkEnableOption "GTK home options";
    disableQt = lib.mkEnableOption "Disable QT settings";

    darkTheme = lib.mkEnableOption "dark theme settings";
    theme = lib.mkOption {
      type = lib.types.enum [ "rose-pine" "catppuccin" "dracula" "tokyonight" "breeze" ];
      default = config.my.home.theme.colorScheme;
      description = lib.mkDoc ''
        Which theme to use with GTK.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    gtk = {
      enable = true;

      cursorTheme = {
        inherit (config.home.pointerCursor) name size package;
      };

      font = {
        name = "Work Sans";
        package = pkgs.work-sans;
        size = 11;
      };

      gtk2 = {
        configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
        extraConfig = ''
          gtk-xft-antialias=1
          gtk-xft-hinting=1
          gtk-xft-hintstyle="hintslight"
          gtk-xft-rgba="rgb"
        '';
        # Add a force here since using Plasma de-symlink-ifies this.
        # See https://github.com/nix-community/home-manager/pull/7073
        force = true;
      };

      gtk3.extraConfig = {
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintslight";
        gtk-xft-rgba = "rgb";

        gtk-application-prefer-dark-theme = if cfg.darkTheme then 1 else 0;
      };

      gtk4.extraConfig = {
        gtk-application-prefer-dark-theme = if cfg.darkTheme then 1 else 0;
      };
    };

    home.pointerCursor = {
      gtk.enable = true;
      x11.enable = true;
    };

    # When this is enabled, KDE and Plasma don't work for Wayland.
    # But this is the style I like for non-KDE stuff, so will re-work later.
    qt = {
      enable = !cfg.disableQt;
      platformTheme.name = "gtk3";
    };

    services.xsettingsd = {
      enable = true;
      settings = {
        "Gtk/CursorThemeName" = "${config.gtk.cursorTheme.name}";
        "Net/IconThemeName" = "${config.gtk.iconTheme.name}";
        "Net/ThemeName" = "${config.gtk.theme.name}";
      };
    };
  };
}
