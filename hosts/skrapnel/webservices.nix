{
  config,
  pkgs,
  lib,
  ...
}:

let
  arrsConfig =
    map
      (name: rec {
        inherit name;
        service = lib.strings.toLower name;
        inherit (config.services."${service}".settings.server) port;
      })
      [
        "Prowlarr"
        "Radarr"
        "Lidarr"
        "Sonarr"
      ];
  jellyfinConfig = {
    name = "Jellyfin";
    service = "jellyfin";
    port = 8096;
  };
  sabnzbdConfig = {
    name = "SABnzbd";
    service = "sabnzbd";
    port = 8080;
  };

  tailscaleHost = "${config.networking.hostName}.gumiho-matrix.ts.net";

  configToLink =
    { name, service, ... }:
    {
      "${name}" = rec {
        href = "https://${tailscaleHost}/${service}/";
        icon = service;
        siteMonitor = href;
      };

    };
in
{
  my.nixos.arrs.enable = true;

  networking.firewall.interfaces."tailscale0".allowedTCPPorts = [
    80
    443
  ];

  services = {
    caddy = {
      enable = true;
      virtualHosts."${tailscaleHost}".extraConfig = ''
        encode
        reverse_proxy localhost:8082

      ''
      + (lib.strings.concatMapStrings
        (
          { service, port, ... }:
          ''
            redir /${service} /${service}/
            handle /${service}/* {
              reverse_proxy localhost:${toString port}
            }
          ''
        )
        (
          arrsConfig
          ++ [
            jellyfinConfig
            sabnzbdConfig
          ]
        )
      )
      + ''

        redir /syncthing /syncthing/
        handle /syncthing/* {
          uri strip_prefix /syncthing
          reverse_proxy ${config.services.syncthing.guiAddress}
        }
      '';
    };

    glances.enable = true;
    homepage-dashboard = {
      enable = true;
      allowedHosts =
        (lib.strings.concatMapStringsSep ","
          (x: "${x}:${toString config.services.homepage-dashboard.listenPort}")
          [
            "localhost"
            "127.0.0.1"
            config.networking.hostName
            "${config.networking.hostName}.lan"
            "${config.networking.hostName}.local"
          ]
        )
        + ",${tailscaleHost}";
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
              columns = 3;
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
                {
                  name = "Info";
                  metric = "info";
                }
                {
                  name = "CPU";
                  metric = "cpu";
                }
                {
                  name = "CPU Temp";
                  metric = "sensor:Package id 0";
                }
                {
                  name = "Processes";
                  metric = "process";
                }
                # Row 2
                {
                  name = "Network";
                  metric = "network:enp2s0";
                }
                {
                  name = "Memory";
                  metric = "memory";
                }
                {
                  name = "Cyberia I/O";
                  metric = "disk:sda";
                }
                {
                  name = "Cyberia Space";
                  metric = "fs:/mnt/Cyberia";
                }
              ];
            in
            map (
              { name, metric }:
              {
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
              }
            ) graphs;
        }
        {
          Arrs = map configToLink arrsConfig;
        }
        {
          Misc =
            (map configToLink [
              jellyfinConfig
              sabnzbdConfig
            ])
            ++ [
              {
                "Resilio Sync" =
                  let
                    port = config.services.resilio.httpListenPort;
                  in
                  rec {
                    href = "http://${config.networking.hostName}:${toString port}";
                    icon = "resiliosync";
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

    resilio = {
      enable = true;
      enableWebUI = true;
      httpListenAddr = "0.0.0.0";
      httpListenPort = 8383;
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

    nix-serve = {
      enable = true;
      package = pkgs.nix-serve-ng;
      openFirewall = true;
      secretKeyFile = config.sops.secrets.cache-priv-key.path;
    };
  };

  sops.secrets.syncthing-password = { };
  sops.secrets.cache-priv-key = { };

  systemd.services.tailscaled.environment.TS_PERMIT_CERT_UID = config.services.caddy.user;
}
