{ pkgs, ... }:

{
  imports =
    [ 
      ../../modules/nix.nix
    ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_rpi4;
    initrd.availableKernelModules = [ "xhci_pci" "usbhid" "usb_storage" ];
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };
    supportedFilesystems = [ "ntfs" ];
  };

  # Fixes cross-compiling for the SD card for the PI.
  # See: https://github.com/NixOS/nixpkgs/issues/126755
  # Also: https://github.com/NixOS/nixpkgs/issues/154163
  nixpkgs.overlays = [
    (final: super: {
      makeModulesClosure = x:
      super.makeModulesClosure (x // { allowMissing = true; });
    })
  ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
      options = [ "noatime" ];
    };
    "/mnt/Backup" = {
      device = "/dev/disk/by-label/Backup";
      fsType = "ntfs-3g";
      options = [ "rw" "uid=1001" ];
    };
  };

  networking = {
    hostName = "raspberrypi";
  };

  environment.systemPackages = with pkgs; [
    vim
    ntfs3g
  ];

  services.openssh.enable = true;

  services.avahi = {
    enable = true;
    nssmdns = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
  };

  users = {
    mutableUsers = false;
    users.pi = {
      isNormalUser = true;
      initialPassword = "changeme";
      uid = 1002;
      extraGroups = [ "wheel" "networkmanager" ];
      openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGg+9LMpvJUBVCndjopRX7Jm6veGyHkf1ZBI/434K2a4" ];
    };
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
