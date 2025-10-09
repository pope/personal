{ config, lib, ... }:

let
  cfg = config.my.nixos.ndi;
in
{
  options.my.nixos.ndi = {
    enable = lib.mkEnableOption "NDI/distroav system options";
  };

  config = lib.mkIf cfg.enable {
    networking.firewall = {
      allowedTCPPortRanges = [
        {
          from = 5959;
          to = 5969;
        }
      ];
      allowedUDPPorts = [ 5353 ];
      allowedUDPPortRanges = [
        {
          from = 5960;
          to = 5969;
        }
      ];
    };

    services.avahi = {
      enable = true;
      publish = {
        enable = true;
        userServices = true;
      };
    };
  };
}
