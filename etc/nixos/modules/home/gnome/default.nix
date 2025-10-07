{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.my.home.gnome;
in
{
  options.my.home.gnome = {
    enable = lib.mkEnableOption "GNOME home options";
    disableGnomeShellExtensions = lib.mkEnableOption "GNOME shell extension options";
  };

  config = lib.mkIf cfg.enable {
    home.packages =
      (with pkgs; [
        adwaita-icon-theme
        gnome-themes-extra
      ])
      ++ lib.optionals (!cfg.disableGnomeShellExtensions) (
        with pkgs.gnomeExtensions;
        [
          app-icons-taskbar
          appindicator
          blur-my-shell
          caffeine
          custom-accent-colors
          forge
          pop-shell
          rounded-window-corners-reborn
          user-themes
        ]
      );

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        # Forcing this since rose-pine can look funky in some apps. It's a
        # jump-scare going back to light background apps.
        gtk-theme = lib.mkForce "Adwaita-dark";
      };
    }
    // lib.optionalAttrs (!cfg.disableGnomeShellExtensions) {
      "org/gnome/shell" = {
        disable-user-extensions = false;

        # `gnome-extensions list` for a list
        enabled-extensions = with pkgs.gnomeExtensions; [
          appindicator.extensionUuid
          blur-my-shell.extensionUuid
          caffeine.extensionUuid
          rounded-window-corners-reborn.extensionUuid
          user-themes.extensionUuid
        ];
      };
      # "org/gnome/shell/extensios/user-theme".name = "Catppuccin-Mocha-Standard-Mauve-dark";
    };
  };
}
