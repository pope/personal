{ config, lib, ... }:

let
  cfg = config.my.nixos.arrs;
  user = config.my.nixos.mainUser;
  inherit (cfg) group;
in
{
  imports = [
    ./sabnzbd.nix
  ];

  options.my.nixos.arrs = {
    enable = lib.mkEnableOption "*arr system options";

    group = lib.mkOption {
      type = lib.types.str;
      default = "wheel";
      description = "The user group to use for the servers";
    };
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
