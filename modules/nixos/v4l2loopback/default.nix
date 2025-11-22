{ config, lib, ... }:
let
  cfg = config.my.nixos.v4l2loopback;
in
{
  options.my.nixos.v4l2loopback = {
    enable = lib.mkEnableOption "Whether to enable v4l2loopback";
  };

  config = lib.mkIf cfg.enable {
    boot = {
      extraModulePackages = [ config.boot.kernelPackages.v4l2loopback ];
      kernelModules = [ "v4l2loopback" ];
    };
  };
}
