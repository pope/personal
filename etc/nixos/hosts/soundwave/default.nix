# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

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
        default = "saved";
        device = "nodev";
        useOSProber = true;
        theme = "${pkgs.p5r-grub}/joker";
        splashImage = "${theme}/background.png";
      };
    };

    initrd = {
      luks.devices."luks-56068f38-928f-450d-b4e4-96be2a27674e" = {
        device = "/dev/disk/by-uuid/56068f38-928f-450d-b4e4-96be2a27674e";
      };

      kernelModules = [ "nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];
      availableKernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
    };

    supportedFilesystems = [ "ntfs" ];

    binfmt.emulatedSystems = [ "aarch64-linux" ];

    # Removes the following warning:
    #     warning: mdadm: Neither MAILADDR nor PROGRAM has been set.
    #     This will cause the `mdmon` service to crash.
    # See https://github.com/NixOS/nixpkgs/issues/254807
    swraid.enable = false;
  };

  fileSystems = {
    "/media/win-cyberia" = {
      device = "/dev/disk/by-label/Cyberia";
      fsType = "ntfs-3g";
      options = [ "rw" "noauto,uid=1000,gid=100" "noatime" ];
    };
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

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
    };

    nvidia = {
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    libva-utils
    renoise343
  ];

  environment.variables = {
    GBM_BACKEND = "nvidia-drm";
    LIBVA_DRIVER_NAME = "nvidia";
    __GLX_VENDOR_LIBRARY_NAME = "nvidia";
  };

  networking = {
    hostName = "soundwave"; # Define your hostname.

    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # proxy = {
    #   default = "http://user:password@proxy:port/";
    #   noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    # Enable networking
    networkmanager.enable = true;

    firewall = {
      enable = true;
      # Open ports in the firewall.
      allowedTCPPorts = [ 8001 ];
      allowedUDPPorts = [ 8001 ];
    };
  };

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  musnix.enable = true;
  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    fah.enable = false;
    fonts.enable = true;
    gaming.enable = true;
    gaming.enableSteam = true;
    onepassword.enable = true;
    printing.enable = true;
    sound.enable = true;
    system.enable = true;
    wayland.enable = true;
    virtualization.enable = true;
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
  system.stateVersion = "24.05"; # Did you read the comment?
}
