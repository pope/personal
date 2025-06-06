{ pkgs, config, lib, ... }:

let
  cfg = config.my.nixos.xserver;
in
{
  config = lib.mkIf (cfg.enable && cfg.hyprland.enable) {
    services = {
      dbus.packages = with pkgs; [ gcr ];
      gnome.gnome-keyring.enable = true;
      gvfs.enable = true;
      tumbler.enable = true;
    };

    security.pam.services.greetd.enableGnomeKeyring = true;

    programs = {
      hyprland = {
        enable = true;
        withUWSM = true;
      };

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
