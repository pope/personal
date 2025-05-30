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
  };
}
