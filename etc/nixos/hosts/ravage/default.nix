# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, pkgs, lib, ... }:

{
  imports =
    [
      inputs.fingerprint-sensor.nixosModules.open-fprintd
      inputs.fingerprint-sensor.nixosModules.python-validity
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480
      self.nixosModules.default
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs.overlays = [
    self.overlays.default
  ];

  boot = {
    # Bootloader.
    loader = {
      systemd-boot.enable = false;
      efi.canTouchEfiVariables = true;
      grub = rec {
        enable = true;
        device = "nodev";
        efiSupport = true;
        extraEntries = ''
          submenu "System" {
            menuentry "Firmware" { fwsetup }
            menuentry "Reboot" { reboot }
            menuentry "Shut Down" { halt }
          }
        '';
        splashImage = "${theme}/background.png";
        theme = "${pkgs.p5r-grub}/navi";
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

  environment.systemPackages = with pkgs; [
    libva-utils
    renoise344
  ];

  # Power management
  services = {
    logind.lidSwitch = "suspend-then-hibernate";
    thermald.enable = true;
    tlp = {
      enable = true;
      settings = {
        PLATFORM_PROFILE_ON_AC = "performance";
        PLATFORM_PROFILE_ON_BAT = "balanced";

        CPU_SCALING_GOVERNOR_ON_AC = "performance";
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
  };
  powerManagement.powertop.enable = true;
  systemd.sleep.extraConfig = ''
    HibernateDelaySec=30m
  '';

  # # Fingerprint
  # services = {
  #   open-fprintd.enable = true;
  #   python-validity.enable = true;
  # };
  security = {
    # pam.services = {
    #   polkit-1.fprintAuth = true;
    #   sudo.fprintAuth = true;
    # };
    wrappers = {
      btop = {
        capabilities = "cap_perfmon=ep";
        group = "wheel";
        owner = "root";
        permissions = "0750";
        source = lib.getExe pkgs.btop;
      };
      intel_gpu_top = {
        capabilities = "cap_perfmon=ep";
        group = "wheel";
        owner = "root";
        permissions = "0750";
        source = lib.getExe' pkgs.intel-gpu-tools "intel_gpu_top";
      };
    };
  };

  my.nixos = {
    mainUser = "pope";

    bluetooth.enable = true;
    flatpak.enable = true;
    fonts = {
      enable = true;
      resolution = "low";
    };
    gaming = {
      enable = true;
      enableSteam = true;
    };
    onepassword.enable = true;
    sound.enable = true;
    system.enable = true;
    users.shell = "zsh";
    xserver = {
      enable = true;
      enableAutoLogin = false;
      displayManager = "gdm";
      dwl.enable = true;
      gnome.enable = true;
    };
    zerotierone.enable = true;
  };

  specialisation.egpu.configuration = {
    imports = [
      inputs.nixos-hardware.nixosModules.common-gpu-amd
    ];
    my.nixos.gpu.amd.enable = true;
    services.hardware.bolt.enable = true;
    system.nixos.tags = [ "eGPU" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
