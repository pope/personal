{ self, inputs, pkgs, ... }:

{
  imports =
    [
      # With this, we can build an SD card for the PI.
      # nix build .#nixosConfigurations.raspberrypi.config.formats.sd-aarch64
      inputs.nixos-generators.nixosModules.all-formats
      self.nixosModules.default
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

  nixpkgs.config.allowUnfree = true;
  nixpkgs.overlays = [
    self.overlays.default
    # Fixes cross-compiling for the SD card for the PI.
    # See: https://github.com/NixOS/nixpkgs/issues/126755
    # Also: https://github.com/NixOS/nixpkgs/issues/154163
    (_final: super: {
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
    git
    ntfs3g
  ];

  services = {
    openssh.enable = true;
    syncthing = {
      enable = false;
      openDefaultPorts = true;
      settings = {
        gui = {
          user = "pi";
          password = "test";
        };
        folders = {
          "Sync" = {
            path = "/mnt/Cyberia/Sync";
          };
        };
      };
    };
  };

  security.sudo.wheelNeedsPassword = false;

  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  my.nixos = {
    mainUser = "pi";

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
    zerotierone.enable = true;
  };

  hardware.enableRedistributableFirmware = true;
  system.stateVersion = "23.11";
}
