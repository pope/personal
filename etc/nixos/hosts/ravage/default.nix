# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, pkgs, lib, ... }:

let
  dwl = (pkgs.dwl.overrideAttrs (oldAttrs: {
    patches = [
      ./ipc.patch
      ./gaps.patch
    ];
  })).override {
    configH = ./config.def.h;
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
  imports =
    [
      inputs.fingerprint-sensor.nixosModules.open-fprintd
      inputs.fingerprint-sensor.nixosModules.python-validity
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    self.overlays.default
  ];

  boot = {
    # Bootloader.
    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;
      grub = rec {
        enable = true;
        efiSupport = true;
        device = "nodev";
        theme = "${pkgs.p5r-grub}/navi";
        splashImage = "${theme}/background.png";
      };
    };

    # Setup keyfile
    initrd = {
      secrets = {
        "/crypto_keyfile.bin" = null;
      };

      # Enable swap on luks
      luks.devices."luks-f38bfaef-5a8e-4850-b5be-7e76ca07785d" = {
        device = "/dev/disk/by-uuid/f38bfaef-5a8e-4850-b5be-7e76ca07785d";
        keyFile = "/crypto_keyfile.bin";
      };
    };

    binfmt.emulatedSystems = [ "aarch64-linux" ];

    # Removes the following warning:
    #     warning: mdadm: Neither MAILADDR nor PROGRAM has been set.
    #     This will cause the `mdmon` service to crash.
    # See https://github.com/NixOS/nixpkgs/issues/254807
    swraid.enable = false;
  };

  networking = {
    hostName = "ravage"; # Define your hostname.
    # wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Enable networking
    networkmanager.enable = true;

    enableIPv6 = false;
    firewall.allowedTCPPorts = [ 8001 ];
  };

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  fileSystems = {
    "/media/cyberia" = {
      device = "raspberrypi.lan:/mnt/Cyberia";
      fsType = "nfs";
      options = [
        "x-systemd.automount"
        "noauto"
        "x-systemd.after=network-online.target"
        "x-systemd.idle-timeout=300"
      ];
    };
  };

  hardware.graphics.enable = true;

  environment = {
    systemPackages = with pkgs; [
      dwl
      dwl-start
      dwlb
      libnotify
      libva-utils
      renoise344
      slstatus
      wdisplays
      wl-clipboard
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
          ExecStart = ''
            ${lib.getExe pkgs.dwlb} -ipc -font 'mono:size=10'
          '';
        };
        bindsTo = [ "dwl-session.target" ];
        wantedBy = [ "dwl-session.target" ];
        restartIfChanged = true;
      };
      status-bar = with pkgs; {
        description = "Service to run the status bar provider";
        enable = true;
        script = "${lib.getExe slstatus} -s | ${lib.getExe dwlb} -status-stdin all -ipc";
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
    displayManager.sessionPackages = [
      ((pkgs.writeTextDir "share/wayland-sessions/dwl.desktop" ''
        [Desktop Entry]
        Name=dwl
        Exec=${lib.getExe dwl-start}
        Type=Application
      '').overrideAttrs (_: { passthru.providedSessions = [ "dwl" ]; }))
    ];
    graphical-desktop.enable = true;
    xserver = {
      enable = true;

      desktopManager.runXdgAutostartIfNone = true;
      displayManager = {
        gdm = {
          enable = true;
          wayland = true;
        };
      };
    };

    # Power management
    logind.lidSwitch = "suspend-then-hibernate";
    thermald.enable = true;
    tlp = {
      enable = true;
      settings = {
        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "balanced";

        CPU_SCALING_GOVERNOR_ON_AC = "ondemand";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;

        DISK_IDLE_SECS_ON_AC = 0;
        DISK_IDLE_SECS_ON_BAT = 2;

        START_CHARGE_THRESH_BAT0 = 85;
        STOP_CHARGE_THRESH_BAT0 = 95;

        START_CHARGE_THRESH_BAT1 = 85;
        STOP_CHARGE_THRESH_BAT1 = 95;

        MEM_SLEEP_ON_AC = "s2idle";
        MEM_SLEEP_ON_BAT = "deep";
      };
    };
  };
  powerManagement.powertop.enable = true;
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
  '';

  # # Fingerprint
  # services = {
  #   open-fprintd.enable = true;
  #   python-validity.enable = true;
  # };
  # security.pam.services = {
  #   sudo.fprintAuth = true;
  #   polkit-1.fprintAuth = true;
  # };

  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    fonts.enable = true;
    gaming.enable = true;
    onepassword.enable = true;
    sound.enable = true;
    system.enable = true;
    xserver = {
      enable = false;

      desktop = "gnome";

      enableAutoLogin = false;
      enableHyprland = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
