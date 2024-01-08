{ config, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
  cfg = config.my.system.samba;
in
{
  options.my.system.samba = {
    enable = mkEnableOption "samba system options";
    shares = mkOption {
      default = { };
      description = "Samba shares";
      type = with types; attrsOf (submodule {
        options = {
          path = mkOption {
            type = str;
          };
        };
      });
    };
  };

  config = mkIf cfg.enable {
    networking = {
      firewall = {
        allowedTCPPorts = [
          5357 # wsdd
        ];
        allowedUDPPorts = [
          3702 # wsdd
        ];
      };
    };

    services = {
      avahi = {
        enable = true;
        nssmdns4 = true;
        publish = {
          enable = true;
          addresses = true;
          domain = true;
          hinfo = true;
          userServices = true;
          workstation = true;
        };
      };

      samba-wsdd.enable = true; # make shares visible for windows 10 clients

      samba =
        let
          inherit (builtins) listToAttrs map getAttr attrNames;
          shareConfig = name:
            let
              share = getAttr name cfg.shares;
            in
            {
              inherit name;
              value = {
                inherit (share) path;
                browseable = "yes";
                "read only" = "no";
                "guest ok" = "no";
                "create mask" = "0644";
                "directory mask" = "0755";
                "force user" = config.my.system.mainUser;
                "force group" = "users";
              };
            };
          shares = listToAttrs (map shareConfig (attrNames cfg.shares));
        in
        {
          inherit shares;
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
        };
    };
  };
}
