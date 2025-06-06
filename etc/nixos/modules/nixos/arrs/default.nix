{ config, lib, ... }:

let
  cfg = config.my.nixos.arrs;
in
{
  options.my.nixos.arrs = {
    enable = lib.mkEnableOption "*arr system options";
  };

  config = lib.mkIf cfg.enable {
    services = {
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
  };
}
