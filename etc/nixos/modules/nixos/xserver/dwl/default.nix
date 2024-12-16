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
      swww-daemon;
    "
  '';
in
{
  config = mkIf (cfg.enable && cfg.dwl.enable) {
    environment.systemPackages = with pkgs; [
      dwl-run
      swww
    ];

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

      services = {
        seatd.enable = false;

        # dwlb = {
        #   description = "Service to run the dwlb status bar";
        #   enable = false;
        #   serviceConfig = {
        #     ExecStart = "/run/current-system/sw/bin/dwlb";
        #   };
        #   bindsTo = [ "dwl-session.target" ];
        #   wantedBy = [ "dwl-session.target" ];
        #   restartIfChanged = true;
        #   reloadTriggers = [ dwlb ];
        #   restartTriggers = [ dwlb ];
        # };

        # status-bar = {
        #   description = "Service to run the status bar provider";
        #   enable = false;
        #   script = ''
        #     /run/current-system/sw/bin/dwl-status \
        #       | /run/current-system/sw/bin/dwlb -status-stdin all
        #   '';
        #   bindsTo = [ "dwlb.service" ];
        #   wantedBy = [ "dwlb.service" ];
        #   reloadTriggers = [ dwlb dwl-status ];
        #   restartTriggers = [ dwlb dwl-status ];
        # };

        polkit-gnome-authentication-agent-1 = {
          description = "polkit-gnome-authentication-agent-1";
          wantedBy = [ "graphical-session.target" ];
          wants = [ "graphical-session.target" ];
          after = [ "graphical-session.target" ];
          serviceConfig = {
            Type = "simple";
            ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
            Restart = "on-failure";
            RestartSec = 1;
            TimeoutStopSec = 10;
          };
        };
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
