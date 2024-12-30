# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, pkgs, ... }:

{
  imports =
    [
      self.nixosModules.default
      inputs.nixos-hardware.nixosModules.common-cpu-intel
      inputs.nixos-hardware.nixosModules.common-pc
      inputs.nixos-hardware.nixosModules.common-pc-ssd
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  nixpkgs = {
    overlays = [
      self.overlays.default
    ];
    config.allowUnfree = true;
  };

  boot = {
    # Bootloader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    hostName = "skrapnel"; # Define your hostname.
    firewall = {
      enable = true;
      allowPing = true;
    };
    networkmanager.enable = true;
  };

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  fileSystems = {
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

  services = {
    nfs.server = {
      enable = true;
      exports = ''
        /mnt/Cyberia    192.168.86.0/24(rw,nohide,insecure,no_subtree_check,all_squash,anonuid=1000,anongid=100)
      '';
    };
    rpcbind.enable = true;
    syncthing = {
      enable = true;
      openDefaultPorts = true;
      settings = {
        gui = {
          user = "pope";
          password = "$2y$10$y83beuzfDJ3L5D/HI2okLe6WXnvj.lNMG7oc27v3Ei/3S4MuZIJou";
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

  my.nixos = {
    mainUser = "pope";

    firewall.nfs.enable = true;
    samba = {
      enable = true;
      shares = {
        Cyberia.path = "/mnt/Cyberia";
      };
    };
    system.enable = true;
    users.shell = "zsh";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
