{ config, lib, pkgs, inputs, ... }:

let
  inherit (lib) mkIf;
  cfg = config.my.nixos.xserver;

  dwlb = pkgs.dwlb.override {
    configH = ./dwlb/config.def.h;
  };

  dwl-status = pkgs.writeShellApplication {
    name = "dwl-status";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      gawk
      gnused
      pamixer
      playerctl
      procps
      upower
    ];
    text = ./status.sh;
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
        dwl-status
        dwl-run
        dwlb
        grim
        imv
        libnotify
        networkmanagerapplet
        pamixer
        playerctl
        procps
        slurp
        swappy
        swww
        udiskie
        wdisplays
        wf-recorder
        wl-clipboard
        wlogout
        wlr-randr
        wmenu

        inputs.anyrun.packages.${system}.default
      ];
      sessionVariables = {
        WLR_NO_HARDWARE_CURSORS = "1";
        # Hint electron apps to use wayland
        NIXOS_OZONE_WL = "1";
      };
    };

    programs = {
      dconf.enable = true;
      light.enable = true;
      uwsm = {
        enable = true;
        waylandCompositors.dwl = {
          prettyName = "dwl";
          comment = "dwl compositor managed by UWSM";
          binPath = "/run/current-system/sw/bin/dwl-run";
        };
      };
      xwayland.enable = true;
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
          enable = false;
          serviceConfig = {
            ExecStart = "/run/current-system/sw/bin/dwlb";
          };
          bindsTo = [ "dwl-session.target" ];
          wantedBy = [ "dwl-session.target" ];
          restartIfChanged = true;
          reloadTriggers = [ dwlb ];
          restartTriggers = [ dwlb ];
        };

        status-bar = {
          description = "Service to run the status bar provider";
          enable = false;
          script = ''
            /run/current-system/sw/bin/dwl-status \
              | /run/current-system/sw/bin/dwlb -status-stdin all
          '';
          bindsTo = [ "dwlb.service" ];
          wantedBy = [ "dwlb.service" ];
          reloadTriggers = [ dwlb dwl-status ];
          restartTriggers = [ dwlb dwl-status ];
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
        extraRules = ''
          ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="acpi_video0", GROUP="video", MODE="0664"
          ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", GROUP="video", MODE="0664"
        '';
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
