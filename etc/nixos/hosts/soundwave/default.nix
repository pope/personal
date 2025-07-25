# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, config, pkgs, lib, ... }:

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
        extraEntries = ''
          submenu "System" {
            menuentry "Firmware" { fwsetup }
            menuentry "Reboot" { reboot }
            menuentry "Shut Down" { halt }
          }
        '';
        gfxmodeBios = "1920x1080";
        gfxmodeEfi = "1920x1080";
        gfxpayloadBios = "keep";
        gfxpayloadEfi = "keep";
        splashImage = "${theme}/background.png";
        theme = "${pkgs.p5r-grub}/joker";
        useOSProber = true;
      };
    };

    supportedFilesystems = [ "ntfs" ];

    binfmt.emulatedSystems = [ "aarch64-linux" ];

    # Removes the following warning:
    #     warning: mdadm: Neither MAILADDR nor PROGRAM has been set.
    #     This will cause the `mdmon` service to crash.
    # See https://github.com/NixOS/nixpkgs/issues/254807
    swraid.enable = false;
  };

  environment.systemPackages = with pkgs; [
    renoise350
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

    enableIPv6 = false;

    firewall = {
      enable = true;
      # Open ports in the firewall.
      allowedTCPPorts = [ 8001 ];
      allowedUDPPorts = [ 8001 ];
    };

    interfaces.enp5s0.wakeOnLan.enable = true;
  };

  services.openssh.extraConfig = ''
    Match User ${config.my.nixos.mainUser}
      ForceCommand systemd-inhibit --who="SSH session" --why="Active user" --what=idle --mode=block ${lib.getExe pkgs.zsh}
  '';

  # Set your time zone.
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  musnix.enable = false;
  my.nixos = {
    mainUser = "pope";

    arrs.enable = false;
    bluetooth.enable = true;
    fah.enable = false;
    flatpak.enable = true;
    fonts.enable = true;
    gaming = {
      enable = true;
      enableSteam = true;
    };
    gpu.nvidia.enable = true;
    ndi.enable = true;
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
      displayManager = "none";
      kde.enable = true;
      gnome.enable = false;
      hyprland.enable = false;
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
