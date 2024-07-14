{ config, lib, ... }:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.v4l2loopback;
in
{
  options.my.nixos.v4l2loopback = {
    enable = mkEnableOption "Whether to enable v4l2loopback";
  };

  config = mkIf cfg.enable {
    boot = {
      extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
      kernelModules = [ "v4l2loopback" ];
    };
  };
}
