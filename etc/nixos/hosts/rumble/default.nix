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
    kernelParams = [
      # From grub-steamos in jupiter-hw-support
      #  - https://github.com/Jovian-Experiments/jupiter-hw-support/blob/08fc462bd0ce0f0c198a5aed36c0bf307fcfb326/etc/default/grub-steamos

      # Valve says:
      #
      # We set amdgpu.lockup_timeout in order to control the TDR for each ring
      # 0 (GFX): 5s (was 10s)
      # 1 (Compute): 10s (was 60s wtf)
      # 2 (SDMA): 10s (was 10s)
      # 3 (Video): 5s (was 10s)

      # ttm.pages_min is set to 8GB in units of page size (4096), which is min
      # required for decent gaming performance.
      # amdgpu.sched_hw_submission is set to 4 to avoid bubbles of lack-of work
      # with the default (2).
      # 4 is the maximum that is supported across RDNA2 + RDNA3.
      # Any more results in a hang at startup on RDNA3.
      "log_buf_len=4M"
      "amd_iommu=off"
      "amdgpu.lockup_timeout=5000,10000,10000,5000"
      "ttm.pages_min=2097152"
      "amdgpu.sched_hw_submission=4"
      "audit=0"
      # Jovian: intentionally not using this one, as many people run
      # setups with LUKS password prompts on fbcon
      # "fbcon=vc:4-6"
      # Jovian: this is Steam Deck specific so it goes into the Deck profile
      # "fbcon=rotate:1"
    ];

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
      displayManager = "gdm";
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
