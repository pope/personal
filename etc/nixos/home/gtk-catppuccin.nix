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

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
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

  qt = {
    enable = true;
    platformTheme = "gtk";
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
