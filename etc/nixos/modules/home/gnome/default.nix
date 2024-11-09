{ pkgs, lib, config, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.gnome;
in
{
  options.my.home.gnome = {
    enable = mkEnableOption "GNOME home options";
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      gnomeExtensions.app-icons-taskbar
      gnomeExtensions.appindicator
      gnomeExtensions.blur-my-shell
      gnomeExtensions.caffeine
      gnomeExtensions.custom-accent-colors
      gnomeExtensions.forge
      gnomeExtensions.pop-shell
      gnomeExtensions.rounded-window-corners-reborn
      gnomeExtensions.user-themes

      gnome-themes-extra
    ];

    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
        # Forcing this since rose-pine can look funky in some apps. It's a
        # jump-scare going back to light background apps.
        gtk-theme = lib.mkForce "Adwaita-dark";
      };

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
