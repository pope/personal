{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.system.gnome;
in
{
  options.my.system.gnome = {
    enable = mkEnableOption "gnome system options";
  };

  config = mkIf cfg.enable {
    services.xserver.desktopManager.gnome.enable = true;
    environment.systemPackages = with pkgs.gnome; [
      adwaita-icon-theme
      gnome-themes-extra
    ];
  };
}
