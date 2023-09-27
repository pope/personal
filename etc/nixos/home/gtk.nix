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

    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
    gtk4.extraCss = ''
      @define-color accent_bg_color #c4a7e7;
      @define-color accent_fg_color #232136;
      @define-color accent_color #c4a7e7;

      @define-color destructive_bg_color #eb6f92; 
      @define-color destructive_fg_color #232136;
      @define-color destructive_color #eb6f92;

      @define-color success_bg_color #9ccfd8;
      @define-color success_fg_color #e0def4;
      @define-color success_color #9ccfd8;

      @define-color warning_bg_color #f6c177;
      @define-color warning_fg_color #e0def4;
      @define-color warning_color #f6c177;

      @define-color error_bg_color #eb6f92;
      @define-color error_fg_color #e0def4;
      @define-color error_color #eb6f92;

      @define-color window_bg_color #232136;
      @define-color window_fg_color #e0def4;

      @define-color view_bg_color #393552;
      @define-color view_fg_color #e0def4;

      @define-color headerbar_bg_color #232136;
      @define-color headerbar_fg_color #e0def4;
      @define-color headerbar_backdrop_color @window_bg_color;
      @define-color headerbar_shade_color #232136;

      @define-color card_bg_color #2a273f;
      @define-color card_fg_color #e0def4;
      @define-color card_shade_color #2a273f;

      @define-color popover_bg_color #393552;
      @define-color popover_fg_color #e0def4;
    '';
  };

  qt = {
    enable = true;
    style.name = "adwaita-dark";
    platformTheme = "gnome";
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
