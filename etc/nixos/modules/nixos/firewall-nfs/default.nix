{ config, lib, ... }:

let
  cfg = config.my.nixos.firewall.nfs;
in
{
  options.my.nixos.firewall.nfs = {
    enable = lib.mkEnableOption "nfs firewall options";
  };

  config = lib.mkIf cfg.enable {
    networking = {
      firewall = {
        allowedTCPPorts = [
          111 #nfs
          2049 #nfs
          20048 # mountd
        ];
        allowedUDPPorts = [
          111 #nfs
          2049 #nfs
          20048 # mountd
        ];
      };
    };
  };
}
