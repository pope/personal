{
  config,
  pkgs,
  lib,
  ...
}:

let
  cfg = config.my.nixos.gpu.amd;
in
{
  options.my.nixos.gpu.amd = {
    enable = lib.mkEnableOption "Common AMD GPU NixOS options";
  };

  config = lib.mkIf cfg.enable {

    nixpkgs.config.rocmSupport = true;

    environment = {
      systemPackages = with pkgs; [
        clinfo
        libva-utils
        rocmPackages.rocm-smi
        rocmPackages.rocminfo
      ];

      variables = {
        # Suppress RADV's "not a conformant Vulkan implementation" stderr warning
        # on newer GPU architectures (e.g., RDNA 4 / GFX12) where upstream Mesa has
        # not yet flipped the Khronos CTS conformance flag.
        MESA_VK_IGNORE_CONFORMANCE_WARNING = "1";
      };
    };

    hardware = {
      amdgpu.opencl.enable = true;

      graphics = {
        enable = true;
        enable32Bit = true;
      };
    };

    services.xserver.videoDrivers = [ "amdgpu" ];
  };
}
