# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ self, inputs, config, lib, ... }:

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
    enableIPv6 = false;
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
    glances.enable = true;
    homepage-dashboard = {
      enable = true;
      allowedHosts = lib.strings.concatMapStringsSep ","
        (x: "${x}:${toString config.services.homepage-dashboard.listenPort}")
        [
          "localhost"
          "127.0.0.1"
          config.networking.hostName
          "${config.networking.hostName}.lan"
          "${config.networking.hostName}.local"
        ];
      openFirewall = true;
      settings = {
        title = "Skrapnel Homepage";
        background = {
          # Shout out to Pawel Czerwinski
          image = "https://images.unsplash.com/photo-1613327986042-63d4425a1a5d?auto=format&fit=crop&w=2560&q=80";
          blur = "md";
        };
        cardBlur = "sm";
        layout = [
          {
            Glances = {
              columns = 4;
              header = true;
              style = "row";
            };
          }
          {
            Arrs = {
              columns = 4;
              header = true;
              style = "row";
            };
          }
          {
            Misc = {
              columns = 2;
              header = true;
              style = "row";
            };
          }
        ];
      };
      services = [
        {
          Glances =
            let
              port = toString config.services.glances.port;
              graphs = [
                # Row 1
                { name = "Info"; metric = "info"; }
                { name = "CPU"; metric = "cpu"; }
                { name = "CPU Temp"; metric = "sensor:Package id 0"; }
                { name = "Processes"; metric = "process"; }
                # Row 2
                { name = "Network"; metric = "network:enp2s0"; }
                { name = "Memory"; metric = "memory"; }
                { name = "Cyberia I/O"; metric = "disk:sda"; }
                { name = "Cyberia Space"; metric = "fs:/mnt/Cyberia"; }
              ];
            in
            map
              ({ name, metric }: {
                "${name}" = {
                  widget = {
                    inherit metric;
                    type = "glances";
                    chart = true;
                    refreshInterval = 5000;
                    url = "http://localhost:${port}";
                    version = 4;
                  };
                };
              })
              graphs;
        }
        {
          Arrs =
            let
              links = [
                { name = "Prowlarr"; service = "prowlarr"; }
                { name = "Radarr"; service = "radarr"; }
                { name = "Lidarr"; service = "lidarr"; }
                { name = "Sonarr"; service = "sonarr"; }
              ];
            in
            map
              ({ name, service }:
                let
                  inherit (config.services.${service}.settings.server) port;
                in
                {
                  "${name}" = rec {
                    href = "http://${config.networking.hostName}:${toString port}";
                    icon = service;
                    siteMonitor = href;
                  };
                })
              links;
        }
        {
          Misc = [
            {
              Jellyfin = rec {
                href = "http://${config.networking.hostName}:8096";
                icon = "jellyfin";
                siteMonitor = href;
              };
            }
            {
              Sabnzbd = rec {
                href = "http://${config.networking.hostName}:8080";
                icon = "sabnzbd";
                siteMonitor = href;
              };
            }
          ];
        }
      ];
    };
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
      enable = true;
      openFirewall = true;
    };
    jellyseerr = {
      enable = false;
      port = 5055;
      openFirewall = true;
    };
  };

  my.nixos = {
    mainUser = "pope";

    arrs.enable = true;
    firewall.nfs.enable = true;
    gpu.intel.enable = true;
    samba = {
      enable = true;
      shares = {
        Cyberia.path = "/mnt/Cyberia";
      };
    };
    sops.enable = true;
    system.enable = true;
    tailscale.enable = true;
    users.shell = "zsh";
    vyprvpn.enable = true;
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
