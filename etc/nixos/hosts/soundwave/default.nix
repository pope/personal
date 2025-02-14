# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, config, pkgs, ... }:

{
  imports =
    [
      inputs.musnix.nixosModules.musnix
      inputs.nixos-hardware.nixosModules.common-cpu-amd
      inputs.nixos-hardware.nixosModules.common-cpu-amd-pstate
      inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
      inputs.nixos-hardware.nixosModules.common-pc
      inputs.nixos-hardware.nixosModules.common-pc-ssd
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    self.overlays.default
  ];
  nixpkgs.config.cudaSupport = true;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    plymouth.enable = true;

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
        gfxmodeBios = "1920x1080";
        gfxmodeEfi = "1920x1080";
        gfxpayloadBios = "keep";
        gfxpayloadEfi = "keep";
        splashImage = "${theme}/background.png";
        theme = "${pkgs.p5r-grub}/joker";
        useOSProber = true;
        extraEntries = ''
          menuentry "Reboot" {
            reboot
          }
          menuentry "Shut Down" {
            halt
          }
          menuentry "Firmware" {
            fwsetup
          }
        '';
      };
    };

    initrd = {
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
    "/media/cyberia" = {
      device = "skrapnel.lan:/mnt/Cyberia";
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
      open = true;
      package = config.boot.kernelPackages.nvidiaPackages.beta;
      powerManagement.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    libva-utils
    renoise344
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

  musnix.enable = false;
  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    fah.enable = false;
    flatpak.enable = true;
    fonts.enable = true;
    gaming.enable = true;
    gaming.enableSteam = true;
    onepassword.enable = true;
    printing.enable = true;
    sound.enable = true;
    system.enable = true;
    users.shell = "zsh";
    v4l2loopback.enable = true;
    virtualization.enable = true;
    wayland.enable = true;
    xserver = {
      enable = true;
      enableAutoLogin = false;
      displayManager = "gdm";
      gnome.enable = true;
      hyprland.enable = true;
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.11"; # Did you read the comment?
}
