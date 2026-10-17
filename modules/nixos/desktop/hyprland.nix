{
  pkgs,
  config,
  lib,
  ...
}:

let
  cfg = config.my.nixos.desktop;
in
{
  config = lib.mkIf (cfg.enable && cfg.hyprland.enable) {
    assertions = [
      {
        assertion = config.hardware.graphics.enable;
        message = "Hardware Graphics must be enabled";
      }
    ];

    hardware.acpilight.enable = true;

    programs = {
      dconf.enable = true;

      hyprland = {
        enable = true;
        withUWSM = true;
      };

      thunar = {
        enable = true;
        plugins = with pkgs; [
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
        ];
      };

      xwayland.enable = true;
    };

    security = {
      pam.services = {
        greetd.enableGnomeKeyring = true;
        hyprlock = { };
      };
      polkit.enable = true;
    };

    services = {
      dbus = {
        enable = true;
        packages = with pkgs; [
          dconf
          gcr_4
        ];
      };
      geoclue2.enable = true;
      gnome.gnome-keyring.enable = true;
      graphical-desktop.enable = true;
      gvfs.enable = true;
      power-profiles-daemon.enable = lib.mkDefault true;
      tumbler.enable = true;
      udev.packages = with pkgs; [
        gnome-settings-daemon
      ];
      udisks2.enable = true;
      upower.enable = true;
      xserver.desktopManager.runXdgAutostartIfNone = true;
    };

    xdg.portal.enable = true;
  };
}
