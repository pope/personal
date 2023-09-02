# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ 
      ../../modules/development.nix
      ../../modules/gaming.nix
      ../../modules/gnome.nix
      ../../modules/sound.nix
      ../../modules/system.nix
      ../../modules/users.nix

      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    # Bootloader.
    loader = {
     systemd-boot.enable = true;
     efi.canTouchEfiVariables = true;
    };

    initrd = {
      # Setup keyfile
      secrets = {
        "/crypto_keyfile.bin" = null;
      };

      # Enable swap on luks
      luks.devices."luks-4ad17370-d029-41e6-9ef0-cf3fec50df26" = {
        device = "/dev/disk/by-uuid/4ad17370-d029-41e6-9ef0-cf3fec50df26";
        keyFile = "/crypto_keyfile.bin";
      };

      kernelModules = ["nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];
      availableKernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
    };

    supportedFilesystems = [ "ntfs" ];
  };

  nixpkgs.overlays = [
    (self: super: {
      renoise343 = super.renoise.override {
        releasePath = /home/pope/Documents/rns_343_linux_x86_64.tar.gz;
      };
    })
  ];

  virtualisation.libvirtd.enable = true;

  hardware = {
    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
      ];
      extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
      setLdLibraryPath = true;
    };

    nvidia = {
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
    };
  };
  services.xserver.videoDrivers = [ "nvidia" ];

  environment.systemPackages = with pkgs; [
    nvidia-vaapi-driver
    renoise343
  ];

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

    # firewall = {
    #   # Open ports in the firewall.
    #   allowedTCPPorts = [ ... ];
    #   allowedUDPPorts = [ ... ];
    #   # Or disable the firewall altogether.
    #   enable = false;
    # };
  };

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  musnix.enable = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
