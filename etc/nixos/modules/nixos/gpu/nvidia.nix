{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.nixos.gpu.nvidia;
in
{
  options.my.nixos.gpu.nvidia = {
    enable = mkEnableOption "Common NVidia GPU NixOS options";
  };
  config = mkIf cfg.enable {
    boot = {
      # I don't believe this is needed if we enable modesetting below.
      # kernelParams = [ "nvidia_drm.modeset=1" ];
      initrd = {
        kernelModules = [ "nvidia" "nvidia_drm" "nvidia_uvm" "nvidia_modeset" ];
        availableKernelModules = [ "nvidia" "nvidia_modeset" "nvidia_uvm" "nvidia_drm" ];
      };
    };

    nixpkgs.config.cudaSupport = true;

    hardware = {
      graphics = {
        enable = true;
        enable32Bit = true;
      };

      nvidia = {
        # Enabling this feature will enable Wayland; however Steam games get
        # pretty laggy, even though the FPS doesn't drop if not if not run in
        # windowed mode. Even still, Steam itself can be janky and glitchy.
        modesetting.enable = true;
        nvidiaSettings = true;
        open = true;
        package = config.boot.kernelPackages.nvidiaPackages.beta;
        powerManagement.enable = true;
      };
    };

    environment.systemPackages = with pkgs; [
      libva-utils
    ];

    environment.variables = {
      GBM_BACKEND = "nvidia-drm";
      LIBVA_DRIVER_NAME = "nvidia";
      __GLX_VENDOR_LIBRARY_NAME = "nvidia";
    };
  };
}
