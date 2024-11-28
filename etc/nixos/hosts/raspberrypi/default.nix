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
    "/mnt/Backup" = {
      device = "/dev/disk/by-label/Cyberia";
      fsType = "ext4";
      options = [ "rw" "users" "noatime" ];
    };
    "/mnt/Cyberia" = {
      device = "/dev/disk/by-label/T5-EVO";
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
    git
    ntfs3g
  ];

  services = {
    nfs.server = {
      enable = true;
      exports = ''
        /mnt/Cyberia    192.168.86.0/24(rw,nohide,insecure,no_subtree_check,all_squash,anonuid=1002,anongid=100)
      '';
    };
    rpcbind.enable = true;
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
    zerotierone = {
      enable = true;
      joinNetworks = [ "272f5eae164a4c0f" ];
    };
  };

  systemd = {
    services.cyberia-backup = {
      description = "Cyberia rsync daily backup service";
      path = with pkgs; [ coreutils-full rsync ];
      script = ''
        LASTBACKUP=$(ls -d /mnt/Backup/Cyberia.*/ | sort | tail -1)
        BACKUP="/mnt/Backup/Cyberia.$(date -d today +"%Y%m%d")"

        if [ -d "$BACKUP" ]; then
          echo "$BACKUP is already backed up. Skipping."
        else
          rsync -av --exclude=lost+found/ --link-dest="$LASTBACKUP" /mnt/Cyberia/ "$BACKUP"
        fi
      '';
      after = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      wants = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      serviceConfig.Type = "oneshot";
    };

    timers.cyberia-backup = {
      description = "Cyberia rsync daily backup timer";
      wantedBy = [ "timers.target" ];
      partOf = [ "cyberia-backup.service" ];
      after = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      requires = [ "mnt-Backup.mount" "mnt-Cyberia.mount" ];
      timerConfig = {
        OnBootSec = "5min";
        OnUnitActiveSec = "6h";
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
