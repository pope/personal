{ pkgs, ... }:

{
  imports =
    [
      ../../modules/nix.nix
    ];

  nix = {
    settings = {
      trusted-users = [ "pi" ];
    };
  };

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
    "/mnt/Cyberia" = {
      device = "/dev/disk/by-label/Cyberia";
      fsType = "ext4";
      options = [ "rw" "users" "noatime" ];
    };
  };

  networking = {
    hostName = "raspberrypi";

    enableIPv6 = false;
    firewall = {
      enable = false;
      allowPing = true;
      allowedTCPPorts = [
        22
        111 2049 #nfs
        5357 # wsdd
      ];
      allowedUDPPorts = [
        22
        111 2049 #nfs
        3702 # wsdd
      ];
    };
  };

  environment.systemPackages = with pkgs; [
    bat
    git
    ntfs3g
    vim
  ];

  services = {
    avahi = {
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
    nfs.server = {
      enable = true;
      exports = ''
        /mnt/Cyberia    192.168.86.0/24(rw,nohide,insecure,no_subtree_check,all_squash,anonuid=1002,anongid=100)
      '';
    };
    openssh.enable = true;
    samba-wsdd.enable = true; # make shares visible for windows 10 clients
    samba = {
      enable = true;
      openFirewall = true;
      securityType = "user";
      extraConfig = ''
        browsable = yes
        smb encrypt = required
        security = user 

        guest account = nobody
        map to guest = bad user
      '';
      shares = {
        Cyberia = {
          path = "/mnt/Cyberia";
          browseable = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "create mask" = "0644";
          "directory mask" = "0755";
          "force user" = "pi";
          "force group" = "users";
        };
      };
    };
  };

  users = {
    # mutableUsers = false;
    users.pi = {
      isNormalUser = true;
      initialPassword = "changeme";
      uid = 1002;
      extraGroups = [ "wheel" "networkmanager" "users" ];
      openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGg+9LMpvJUBVCndjopRX7Jm6veGyHkf1ZBI/434K2a4" ];
    };
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
