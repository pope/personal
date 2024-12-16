{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.xserver;

  dwl-run = pkgs.writeShellScriptBin "dwl-run" ''
    HOME_DWL_EXE=/etc/profiles/per-user/${mainUser}/bin/dwl
    DWL_EXE=$(lib.getExe pkgs.dwl)
    if [ -f $HOME_DWL_EXE ]
    then
      DWL_EXE=$HOME_DWL_EXE
    fi

    exec $DWL_EXE -s "
      dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user start dwl-session.target;
    "
  '';
in
{
  config = mkIf (cfg.enable && cfg.dwl.enable) {
    environment.systemPackages = [ dwl-run ];

    programs = {
      uwsm = {
        enable = true;
        waylandCompositors.dwl = {
          prettyName = "dwl";
          comment = "dwl compositor managed by UWSM";
          binPath = "/run/current-system/sw/bin/dwl-run";
        };
      };
    };

    xdg.portal = {
      wlr.enable = true;
      config.dwl.default = [ "wlr" "gtk" ];
      config.common.default = [ "wlr" ];
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-wlr
      ];
    };

    systemd.user = {
      targets.dwl-session = {
        documentation = [ "man:systemd.special(7)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
      };
    };

    services = {
      udev.extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", GROUP="video", MODE="0664"
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", GROUP="video", MODE="0664"
      '';
      xserver.desktopManager.runXdgAutostartIfNone = true;
    };
  };
}
