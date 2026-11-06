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
      # Tune here instead of in the overlay so we don't rebuild everything that
      # depends on mesa.
      package = pkgs.withCpuTuning pkgs.mesa;
      package32 = lib.mkIf config.hardware.graphics.enable32Bit (
        pkgs.withCpuTuning pkgs.pkgsi686Linux.mesa
      );
    };
  };
}
