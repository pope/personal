# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ ... }:

{
  imports =
    [
      ../../modules/nixos
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    initrd = {
      luks.devices."luks-b3a5fbc2-0e7e-4b6a-9019-37629c91d744" = {
        device = "/dev/disk/by-uuid/b3a5fbc2-0e7e-4b6a-9019-37629c91d744";
      };
    };

    supportedFilesystems = [ "ntfs" ];

    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };

  networking = {
    hostName = "rumble";

    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    networkmanager.enable = true;
  };

  time.timeZone = "America/Los_Angeles";

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

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  services.fwupd.enable = true;
  # Must be explicitly false otherwise there's infinite recursion going on.
  services.tlp.enable = false;

  musnix.enable = true;

  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    fonts.enable = true;
    gaming.enable = true;
    gaming.enableSteam = true;
    onepassword.enable = true;
    printing.enable = true;
    sound.enable = true;
    system.enable = true;
    users.shell = "zsh";
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
