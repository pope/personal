{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.nixos.gpu.intel;
in
{
  options.my.nixos.gpu.intel = {
    enable = lib.mkEnableOption "Common Intel GPU NixOS options";
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = config.hardware.intelgpu.driver == "i915";
        message = "NixOS Hardware should have enabled intel GPU settings";
      }
    ];

    hardware.graphics.enable = true;

    security.wrappers = {
      btop = {
        capabilities = "cap_perfmon=ep";
        group = "wheel";
        owner = "root";
        permissions = "0750";
        source = lib.getExe pkgs.btop;
      };
      intel_gpu_top = {
        capabilities = "cap_perfmon=ep";
        group = "wheel";
        owner = "root";
        permissions = "0750";
        source = lib.getExe' pkgs.intel-gpu-tools "intel_gpu_top";
      };
    };
  };
}
