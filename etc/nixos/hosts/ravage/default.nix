# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ pkgs, ... } @ args:

let
  overlays = import ../../overlays args;
in
{
  imports =
    [
      ../../modules
      ../../modules/bluetooth.nix
      ../../modules/display-manager.nix
      ../../modules/gnome.nix
      ../../modules/hyprland.nix
      ../../modules/nix.nix
      ../../modules/system.nix
      ../../modules/users.nix

      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = with overlays; [
    waybar
    plow
  ];

  boot = {
    # Bootloader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
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

  hardware = {
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver # LIBVA_DRIVER_NAME=iHD
        vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
        vaapiVdpau
        libvdpau-va-gl
      ];
    };
  };

  services = {
    # Power management
    power-profiles-daemon.enable = true;
    thermald.enable = true;
    tlp.enable = false; # Conflicts with PPD above. But maybe useful in the future.

    openssh.enable = true;
    xserver.displayManager.defaultSession = "hyprland";
  };
  powerManagement.powertop.enable = true;

  my.system = {
    gaming.enable = true;
    sound.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
