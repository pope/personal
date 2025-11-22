{ config, lib, ... }:

let
  cfg = config.my.nixos.nfs.host;
in
{
  options.my.nixos.nfs.host = {
    enable = lib.mkEnableOption "NFS host system options";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPorts = [
        111 # nfs
        2049 # nfs
        20048 # mountd
      ];
      allowedUDPPorts = [
        111 # nfs
        2049 # nfs
        20048 # mountd
      ];
    };

    services = {
      nfs.server = {
        enable = true;
        exports =
          let
            opts = "rw,nohide,insecure,no_subtree_check,all_squash,anonuid=1000,anongid=100";
          in
          ''
            /mnt/Cyberia 192.168.86.0/24(${opts}) 100.0.0.0/8(${opts})
          '';
      };
      rpcbind.enable = true;
    };
  };
}
