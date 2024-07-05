{ inputs, pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.enableHyprland) {
    services = {
      dbus = {
        enable = true;
        packages = with pkgs; [ gcr dconf ];
      };
      geoclue2.enable = true;
      gnome.gnome-keyring.enable = true;
      gvfs.enable = true;
      power-profiles-daemon.enable = !config.services.tlp.enable;
      tumbler.enable = true;
      upower.enable = true;
      udev = {
        packages = with pkgs; [
          gnome.gnome-settings-daemon
        ];
      };
      udisks2.enable = true;
      xserver.enable = true;
    };

    security.pam.services.greetd.enableGnomeKeyring = true;
    security.pam.services.hyprlock = {};

    xdg.portal.enable = true;

    programs = {
      hyprland = {
        enable = true;
        package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      };

      light.enable = true;

      thunar = {
        enable = true;
        plugins = with pkgs.xfce; [
          thunar-archive-plugin
          thunar-media-tags-plugin
          thunar-volman
        ];
      };
    };

    environment = {
      systemPackages = with pkgs; [
        xfce.ristretto
        xfce.thunar
      ];
    };
  };
}
