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

    environment.systemPackages = with pkgs; [
      libva-utils
      rocmPackages.rocm-smi
      rocmPackages.rocminfo
    ];

    hardware = {
      graphics = {
        enable = true;
        enable32Bit = true;
        extraPackages = with pkgs; [
          rocmPackages.clr.icd
          # Encoding/decoding acceleration
          stable.libvdpau-va-gl
          libva-vdpau-driver
          libva
        ];
      };
    };
  };
}
