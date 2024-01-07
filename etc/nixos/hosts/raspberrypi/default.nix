{ pkgs, ... } @ args:

let
  overlays = import ../../overlays args;
in
{
  imports =
    [
      ../../modules
      ../../modules/firewall-nfs.nix
      ../../modules/nix.nix
      ../../modules/samba.nix
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
    binfmt.emulatedSystems = [ "x86_64-linux" ];
  };

  nixpkgs.overlays = with overlays; [
    # Fixes cross-compiling for the SD card for the PI.
    # See: https://github.com/NixOS/nixpkgs/issues/126755
    # Also: https://github.com/NixOS/nixpkgs/issues/154163
    (_final: super: {
      makeModulesClosure = x:
        super.makeModulesClosure (x // { allowMissing = true; });
    })
    waybar
    plow
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
      enable = true;
      allowPing = true;
      allowedTCPPorts = [
        22
      ];
      allowedUDPPorts = [
        22
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
    nfs.server = {
      enable = true;
      exports = ''
        /mnt/Cyberia    192.168.86.0/24(rw,nohide,insecure,no_subtree_check,all_squash,anonuid=1002,anongid=100)
      '';
    };
    openssh.enable = true;
    samba = {
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

  security.sudo.wheelNeedsPassword = false;

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
