{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.gnome;
in
{
  options.my.nixos.gnome = {
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
