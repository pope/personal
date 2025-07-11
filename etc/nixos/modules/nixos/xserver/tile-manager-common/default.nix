{ pkgs, config, lib, ... }:

let
  cfg = config.my.nixos.xserver;
in
{
  config = lib.mkIf (cfg.enable && (cfg.dwl.enable || cfg.hyprland.enable)) {
    assertions = [
      {
        assertion = config.hardware.graphics.enable;
        message = "Hardware Graphics must be enabled";
      }
    ];

    programs = {
      dconf.enable = true;
      light.enable = true;
      xwayland.enable = true;
    };

    security = {
      pam.services.hyprlock = { };
      polkit.enable = true;
    };

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
