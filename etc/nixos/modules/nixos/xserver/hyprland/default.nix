{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.hyprland.enable) {
    services = {
      dbus.packages = with pkgs; [ gcr ];
      gnome.gnome-keyring.enable = true;
      gvfs.enable = true;
      tumbler.enable = true;
    };

    security.pam.services.greetd.enableGnomeKeyring = true;
    security.pam.services.hyprlock = { };

    programs = {
      hyprland.enable = true;

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
