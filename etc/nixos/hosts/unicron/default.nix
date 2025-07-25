# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, pkgs, ... }:

{
  imports =
    [
      inputs.musnix.nixosModules.musnix
      inputs.nixos-hardware.nixosModules.common-cpu-amd
      inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
      inputs.nixos-hardware.nixosModules.common-gpu-amd
      inputs.nixos-hardware.nixosModules.common-pc
      inputs.nixos-hardware.nixosModules.common-pc-ssd
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    self.overlays.default
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    # Bootloader.
    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;
      grub = rec {
        enable = true;

        configurationLimit = 10;
        default = "saved";
        device = "nodev";
        efiSupport = true;
        extraEntries = ''
          menuentry "Firmware" { fwsetup }
          submenu "Reboot / Shutdown" {
            menuentry "Reboot" { reboot }
            menuentry "Shut Down" { halt }
          }
        '';
        splashImage = "${theme}/background.png";
        theme = "${pkgs.p5r-grub}/joker";
        useOSProber = true;
      };
    };

    # resumeDevice = (builtins.head config.swapDevices).device;
    #
    supportedFilesystems = [ "ntfs" ];

    binfmt.emulatedSystems = [ "aarch64-linux" ];

    # Removes the following warning:
    #     warning: mdadm: Neither MAILADDR nor PROGRAM has been set.
    #     This will cause the `mdmon` service to crash.
    # See https://github.com/NixOS/nixpkgs/issues/254807
    swraid.enable = false;
  };

  fileSystems = {
    # "/media/win-cyberia" = {
    #   device = "/dev/disk/by-label/Cyberia";
    #   fsType = "ntfs-3g";
    #   options = [ "rw" "noauto,uid=1000,gid=100" "noatime" ];
    # };
    "/media/games" = {
      device = "/dev/nvme1n1p4";
      fsType = "ntfs-3g";
      options = [ "rw" "uid=pope" ];
    };
  };
  environment.systemPackages = with pkgs; [
    renoise350
  ];

  services.xserver.videoDrivers = [ "amdgpu" ];

  networking = {
    hostName = "unicron"; # Define your hostname.

    # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    # Configure network proxy if necessary
    # proxy = {
    #   default = "http://user:password@proxy:port/";
    #   noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    # Enable networking
    networkmanager.enable = true;

    enableIPv6 = false;

    firewall.enable = true;

    interfaces.eno1.wakeOnLan.enable = true;
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
    flatpak.enable = true;
    fonts = {
      enable = true;
      resolution = "high";
    };
    gaming = {
      enable = true;
      enableSteam = true;
      preferredOutput = "HDMI-A-2";
    };
    gpu.amd.enable = true;
    nfs.client.enable = true;
    onepassword.enable = true;
    printing.enable = true;
    sops.enable = true;
    sound.enable = true;
    system.enable = true;
    tailscale.enable = true;
    users.shell = "zsh";
    v4l2loopback.enable = true;
    virtualization.enable = true;
    xserver = {
      enable = true;
      enableAutoLogin = false;
      displayManager = "sddm";
      gnome.enable = false;
      kde.enable = true;
      hyprland.enable = true;
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
