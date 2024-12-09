{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;

  dwlb = pkgs.dwlb.override {
    configH = ./dwlb/config.def.h;
  };

  slstatus = pkgs.slstatus.override {
    conf = ./slstatus/config.def.h;
  };

  dwl = (pkgs.dwl.overrideAttrs (_oldAttrs: {
    patches = [
      ./dwl/patches/ipc.patch
      ./dwl/patches/gaps.patch
      ./dwl/patches/alwayscenter.patch
    ];
  })).override {
    configH = ./dwl/config.def.h;
  };

  dwl-run = pkgs.writeShellScriptBin "dwl-run" ''
    exec ${lib.getExe dwl} -s "
      dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user start dwl-session.target;
      swww-daemon;
    "
  '';
in
{
  config = mkIf (cfg.enable && cfg.desktop == "dwl") {
    hardware.graphics.enable = true;

    environment = {
      systemPackages = with pkgs; [
        alsa-utils
        brillo
        dwl
        dwl-run
        dwlb
        grim
        imv
        libnotify
        networkmanagerapplet
        pamixer
        slstatus
        slurp
        swappy
        swww
        udiskie
        wdisplays
        wf-recorder
        wl-clipboard
        wlr-randr
        wmenu
      ];
      sessionVariables = {
        WLR_NO_HARDWARE_CURSORS = "1";
        # Hint electron apps to use wayland
        NIXOS_OZONE_WL = "1";
      };
    };

    programs = {
      dconf.enable = true;
      xwayland.enable = true;
      uwsm = {
        enable = true;
        waylandCompositors.dwl = {
          prettyName = "dwl";
          comment = "dwl compositor managed by UWSM";
          binPath = "/run/current-system/sw/bin/dwl-run";
        };
      };
    };

    security.polkit.enable = true;

    xdg.portal = {
      enable = true;
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

        dwlb = {
          description = "Service to run the dwlb status bar";
          enable = true;
          serviceConfig = {
            ExecStart = "/run/current-system/sw/bin/dwlb";
          };
          bindsTo = [ "dwl-session.target" ];
          wantedBy = [ "dwl-session.target" ];
          restartIfChanged = true;
        };

        status-bar = {
          description = "Service to run the status bar provider";
          enable = true;
          script = ''
            /run/current-system/sw/bin/slstatus -s \
              | /run/current-system/sw/bin/dwlb -status-stdin all -ipc
          '';
          bindsTo = [ "dwlb.service" ];
          wantedBy = [ "dwlb.service" ];
          reloadTriggers = [ dwlb slstatus ];
          restartTriggers = [ dwlb slstatus ];
        };

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
      dbus = {
        enable = true;
        packages = with pkgs; [ dconf ];
      };

      graphical-desktop.enable = true;
      geoclue2.enable = true;
      udev = {
        packages = with pkgs; [
          gnome-settings-daemon
        ];
      };
      udisks2.enable = true;
      upower.enable = true;

      xserver = {
        desktopManager.runXdgAutostartIfNone = true;

        displayManager = {
          gdm = {
            enable = true;
            wayland = true;
          };
        };
      };
    };
  };
}
