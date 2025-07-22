{ config, lib, ... }:

{
  my.nixos.arrs.enable = true;

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 80 443 ];

  services = {
    caddy = {
      enable = true;
      virtualHosts."skrapnel.gumiho-matrix.ts.net".extraConfig = ''
        encode
        reverse_proxy localhost:8082

        redir /radarr /radarr/
        handle /radarr/* {
          uri strip_prefix /radarr
          reverse_proxy localhost:${toString config.services.radarr.settings.server.port}
        }

        redir /syncthing /syncthing/
        handle /syncthing/* {
          uri strip_prefix /syncthing
          reverse_proxy localhost:8384
        }
      '';
    };

    glances.enable = true;
    homepage-dashboard = {
      enable = true;
      allowedHosts = (lib.strings.concatMapStringsSep ","
        (x: "${x}:${toString config.services.homepage-dashboard.listenPort}")
        [
          "localhost"
          "127.0.0.1"
          config.networking.hostName
          "${config.networking.hostName}.lan"
          "${config.networking.hostName}.local"
        ]) + ",${config.networking.hostName}.gumiho-matrix.ts.net";
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

    syncthing = {
      enable = true;
      guiAddress = "0.0.0.0:8384";
      openDefaultPorts = true;
      settings = {
        gui = {
          user = "pope";
          password = config.sops.secrets.syncthing-password;
        };
        folders = {
          "Sync" = {
            path = "/mnt/Cyberia/Sync";
          };
        };
      };
    };
  };

  sops.secrets.syncthing-password = { };

  systemd.services.tailscaled.environment.TS_PERMIT_CERT_UID = config.services.caddy.user;
}
