# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, ... }:

{
  imports =
    [
      self.nixosModules.default
      inputs.nixos-hardware.nixosModules.common-cpu-intel
      inputs.nixos-hardware.nixosModules.common-gpu-intel
      inputs.nixos-hardware.nixosModules.common-pc
      inputs.nixos-hardware.nixosModules.common-pc-ssd
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./backup.nix
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

    owncast = {
      enable = true;
      listen = "0.0.0.0";
      openFirewall = true;
      port = 8088;
    };

    # # Jellyfin
    jellyfin = {
      enable = false;
      openFirewall = true;
    };
    jellyseerr = {
      enable = false;
      port = 5055;
      openFirewall = true;
    };
    sabnzbd = {
      enable = true;
      openFirewall = true;
      user = "pope";
      group = "wheel";
    };
    sonarr = {
      enable = true;
      openFirewall = true;
      user = "pope";
      group = "wheel";
    };
    radarr = {
      enable = true;
      openFirewall = true;
      user = "pope";
      group = "wheel";
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
    zerotierone.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
