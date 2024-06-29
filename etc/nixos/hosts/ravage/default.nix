# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... }:

{
  imports =
    [
      ../../modules/nixos
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
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

  environment.systemPackages = with pkgs; [
    libva-utils
    renoise343
  ];
  musnix.enable = true;

  services = {
    # Power management
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

    openssh.enable = true;
  };
  powerManagement.powertop.enable = true;

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
      enable = true;

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
