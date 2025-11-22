{ config, lib, ... }:

let
  cfg = config.my.nixos.arrs;
  user = config.my.nixos.mainUser;
  group = "wheel";
in
{
  options.my.nixos.arrs = {
    enable = lib.mkEnableOption "*arr system options";
  };

  config = lib.mkIf cfg.enable {
    services = {
      lidarr = {
        inherit group user;
        enable = true;
        openFirewall = true; # 8686
      };
      prowlarr = {
        enable = true;
        openFirewall = true; # 9696
      };
      radarr = {
        inherit group user;
        enable = true;
        openFirewall = true; # 7878
      };
      readarr = {
        inherit group user;
        enable = false;
        openFirewall = true; # 8787
      };
      sabnzbd = {
        inherit group user;
        enable = true;
        openFirewall = true; # 8080
      };
      sonarr = {
        inherit group user;
        enable = true;
        openFirewall = true; # 8989
      };
      whisparr = {
        inherit group user;
        enable = false;
        openFirewall = true; # 6969
      };
    };
  };
}
