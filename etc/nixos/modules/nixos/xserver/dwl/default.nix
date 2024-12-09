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
    ];
  })).override {
    configH = ./dwl/config.def.h;
  };
  dwl-start = pkgs.writeShellScriptBin "dwl-start" ''
    set -x

    systemctl --user is-active dwl-session.target \
      && echo "DWL is already running" \
      && exit 1

    # The commands below were adapted from:
    # https://github.com/NixOS/nixpkgs/blob/ad3e815dfa9181aaa48b9aa62a00cf9f5e4e3da7/nixos/modules/programs/wayland/sway.nix#L122
    # Import the most important environment variables into the D-Bus and systemd
    dbus-run-session -- ${lib.getExe dwl} -s "
      dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user import-environment DISPLAY WAYLAND_DISPLAY XDG_SESSION_TYPE;
      systemctl --user start dwl-session.target;
    " &
    dwlPID=$!
    wait $dwlPID
    systemctl --user stop dwl-session.target
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
        dwl-start
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
        seatd.enable = true;

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

        swww = {
          description = "Efficient animated wallpaper daemon for wayland";
          enable = true;
          wantedBy = [ "graphical-session.target" ];
          wants = [ "graphical-session.target" ];
          after = [ "graphical-session.target" ];
          serviceConfig = {
            ExecStart = "${pkgs.swww}/bin/swww-daemon";
            ExecStop = "${pkgs.swww}/bin/swww kill";
            Restart = "always";
            RestartSec = 10;
          };
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

      displayManager.sessionPackages = [
        ((pkgs.writeTextDir "share/wayland-sessions/dwl.desktop" ''
          [Desktop Entry]
          Name=dwl
          Exec=/run/current-system/sw/bin/dwl-start
          Type=Application
        '').overrideAttrs (_: { passthru.providedSessions = [ "dwl" ]; }))
      ];

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
