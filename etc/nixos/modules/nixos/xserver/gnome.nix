{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && cfg.desktop == "gnome") {
    services.xserver = {
      desktopManager.gnome.enable = true;
      displayManager.gdm.enable = true;
    };

    environment.systemPackages = with pkgs.gnome; [
      adwaita-icon-theme
      gnome-themes-extra
    ];
  };
}
