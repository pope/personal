{ pkgs, config, lib, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;
in
{
  config = mkIf (cfg.enable && (cfg.dwl.enable || cfg.hyprland.enable)) {
    hardware.graphics.enable = true;

    programs = {
      dconf.enable = true;
      light.enable = true;
      xwayland.enable = true;
    };

    security.polkit.enable = true;

    services = {
      dbus = {
        enable = true;
        packages = with pkgs; [ dconf ];
      };
      geoclue2.enable = true;
      graphical-desktop.enable = true;
      power-profiles-daemon.enable = !config.services.tlp.enable;
      udev.packages = with pkgs; [
        gnome-settings-daemon
      ];
      udisks2.enable = true;
      upower.enable = true;
      xserver.desktopManager.runXdgAutostartIfNone = true;
    };

    systemd.user = {
      targets.tile-manager-session = {
        documentation = [ "man:systemd.special(7)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
      };
    };

    xdg.portal.enable = true;
  };

}
