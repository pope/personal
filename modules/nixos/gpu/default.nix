{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./amd.nix
    ./intel.nix
    ./nvidia.nix
  ];

  config = lib.mkIf config.hardware.graphics.enable {
    hardware.graphics = {
      package = pkgs.withV3 pkgs.mesa;
      package32 = lib.mkIf config.hardware.graphics.enable32Bit (pkgs.withV3 pkgs.pkgsi686Linux.mesa);
    };
  };
}
