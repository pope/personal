{ pkgs, self, ... }:

{
  imports =
    [
      ../../modules/nixos
    ];

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

  nixpkgs.overlays = [
    # Fixes cross-compiling for the SD card for the PI.
    # See: https://github.com/NixOS/nixpkgs/issues/126755
    # Also: https://github.com/NixOS/nixpkgs/issues/154163
    (_final: super: {
      makeModulesClosure = x:
        super.makeModulesClosure (x // { allowMissing = true; });
    })
    self.overlays.default
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
  };

  security.sudo.wheelNeedsPassword = false;

  my.nixos = {
    mainUser = "pi";

    firewall.nfs.enable = true;
    samba = {
      enable = true;
      shares = {
        Cyberia.path = "/mnt/Cyberia";
      };
    };
    users = {
      # TODO(pope): Is `mutableUsers = false;` a good idea?
      uid = 1002;
      initialPassword = "changeme";
    };
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
