{ config, lib, ... }:
let
  cfg = config.my.nixos.zerotierone;
in
{
  options.my.nixos.zerotierone = {
    enable = lib.mkEnableOption "Whether to enable zerotierone";
  };

  config = lib.mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = [ "272f5eae164a4c0f" ];
    };

    networking.hosts = {
      "192.168.195.154" = [ "shifteleven.zero" ];
      "192.168.195.177" = [ "soundwave.zero" ];
      "192.168.195.185" = [ "unicron.zero" ];
      "192.168.195.186" = [ "ravage.zero" ];
      "192.168.195.35" = [ "rumble.zero" ];
      "192.168.195.44" = [ "galvatron.zero" ];
      "192.168.195.88" = [ "skrapnel.zero" ];
    };
  };
}
