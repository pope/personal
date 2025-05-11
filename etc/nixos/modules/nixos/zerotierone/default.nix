{ config, lib, ... }:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.zerotierone;
in
{
  options.my.nixos.zerotierone = {
    enable = mkEnableOption "Whether to enable zerotierone";
  };

  config = mkIf cfg.enable {
    services.zerotierone = {
      enable = true;
      joinNetworks = [ "272f5eae164a4c0f" ];
    };
  };
}
