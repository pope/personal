{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
  inherit (config.my.nixos) mainUser;
  cfg = config.my.nixos.virtualization;
in
{
  config = mkIf (cfg.enable && cfg.kind == "host") {
    environment.systemPackages = with pkgs; [
      qemu
      virtiofsd
    ];
    programs.virt-manager.enable = true;
    virtualisation = {
      libvirtd = {
        enable = true;
        qemu.ovmf.enable = true;
      };
      spiceUSBRedirection.enable = true;
    };
    users.users."${mainUser}".extraGroups = [ "libvirtd" "kvm" ];
  };
}
