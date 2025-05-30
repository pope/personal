{ config, lib, ... }:

let
  cfg = config.my.nixos.samba;
in
{
  options.my.nixos.samba = {
    enable = lib.mkEnableOption "samba system options";
    shares = lib.mkOption {
      default = { };
      description = "Samba shares";
      type = with lib.types; attrsOf (submodule {
        options = {
          path = lib.mkOption {
            type = str;
          };
        };
      });
    };
  };

  config = lib.mkIf cfg.enable {
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
                "force user" = config.my.nixos.mainUser;
                "force group" = "users";
              };
            };
          shares = listToAttrs (map shareConfig (attrNames cfg.shares));
        in
        {
          enable = true;
          openFirewall = true;
          settings = {
            global = {
              browsable = "yes";
              "smb encrypt" = "required";
              security = "user";

              "guest account" = "nobody";
              "map to guest" = "bad user";
            };
          } // shares;
        };
    };
  };
}
