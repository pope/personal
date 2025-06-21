# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, pkgs, lib, ... }:

{
  imports =
    [
      inputs.musnix.nixosModules.musnix
      inputs.nixos-hardware.nixosModules.framework-13-7040-amd
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nix.settings.system-features = [ "gccarch-znver4" ];
  # nixpkgs.hostPlatform = {
  #   gcc.arch = "znver4";
  #   gcc.tune = "znver4";
  #   system = "x86_64-linux";
  # };

  nixpkgs.overlays = [
    self.overlays.default
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
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

    enableIPv6 = false;
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
  };

  time.timeZone = "America/Los_Angeles";

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
    framework = {
      enableKmod = true;
      laptop13.audioEnhancement.enable = true;
    };

    framework.amd-7040.preventWakeOnAC = true;
  };

  services = {
    fwupd = {
      enable = true;
      extraRemotes = [ "lvfs-testing" ]; # Some framework firmware is still in testing
    };

    # Must be explicitly false otherwise there's infinite recursion going on.
    tlp.enable = false;
    logind.lidSwitch = "suspend-then-hibernate";
    logind.lidSwitchExternalPower = "suspend";
    power-profiles-daemon.enable = true;
  };

  powerManagement = {
    enable = true;
    cpuFreqGovernor = lib.mkDefault "powersave";
    powertop.enable = true; # Run powertop on boot
  };

  systemd.sleep.extraConfig = ''
    AllowHibernation=yes
    AllowHybridSleep=yes
    AllowSuspend=yes
    AllowSuspendThenHibernate=yes
    HibernateDelaySec=4h
    MemorySleepMode=s2idle
  '';

  environment.systemPackages = with pkgs; [
    renoise344
  ];

  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    flatpak.enable = true;
    fonts = {
      enable = true;
      resolution = "high";
    };
    gaming = {
      enable = true;
      enableSteam = true;
    };
    gpu.amd.enable = true;
    onepassword.enable = true;
    printing.enable = true;
    sops.enable = true;
    sound.enable = true;
    system.enable = true;
    users.shell = "zsh";
    v4l2loopback.enable = true;
    virtualization.enable = true;
    xserver = {
      enable = true;
      enableAutoLogin = false;
      displayManager = "sddm";
      dwl.enable = true;
      gnome.enable = false;
      kde.enable = true;
      hyprland.enable = false;
    };
    vyprvpn.enable = true;
    zerotierone.enable = true;
  };

  specialisation = {
    music.configuration = {
      system.nixos.tags = [ "music" ];
      musnix.enable = true;
      boot.extraModprobeConfig = "options snd-virmidi index=-2 midi_devs=1";
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
