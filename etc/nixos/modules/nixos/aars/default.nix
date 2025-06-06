{ config, lib, ... }:

let
  cfg = config.my.nixos.aars;
in
{
  options.my.nixos.aars = {
    enable = lib.mkEnableOption "*aar system options";
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
