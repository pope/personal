{ pkgs, config, ... }:

rec {
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
      name = "Iosevka";
      package = pkgs.iosevka;
      size = 12;
    };

    cursorTheme = {
      inherit (home.pointerCursor) name size package;
    };

    iconTheme = {
      name = "rose-pine-moon";
      package = pkgs.rose-pine-icon-theme;
    };

    theme = {
      name = "rose-pine-moon";
      package = pkgs.rose-pine-gtk-theme;
    };

    gtk2.configLocation = "${config.xdg.configHome}/gtk-2.0/gtkrc";
    gtk2.extraConfig = ''
      gtk-xft-antialias=1
      gtk-xft-hinting=1
      gtk-xft-hintstyle="hintslight"
      gtk-xft-rgba="rgb"
    '';

    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;

      gtk-xft-antialias = 1;
      gtk-xft-hinting = 1;
      gtk-xft-hintstyle = "hintslight";
      gtk-xft-rgba = "rgb";
    };

    gtk4.extraConfig = { gtk-application-prefer-dark-theme = 1; };

  };

  services.xsettingsd = {
    enable = true;
    settings = {
      "Gtk/CursorThemeName" = "${gtk.cursorTheme.name}";
      "Net/IconThemeName" = "${gtk.iconTheme.name}";
      "Net/ThemeName" = "${gtk.theme.name}";
    };
  };
}
