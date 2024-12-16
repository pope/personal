{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.my.home.dwl;

  dwlb = pkgs.dwlb.override {
    configH = ./dwlb/config.def.h;
  };

  dwl-status = pkgs.writeShellApplication {
    name = "dwl-status";
    runtimeInputs = with pkgs; [
      bash
      coreutils
      gawk
      gnused
      pamixer
      playerctl
      procps
      upower
    ];
    text = ./status.sh;
  };

  dwl = (pkgs.dwl.overrideAttrs (_oldAttrs: {
    patches = [
      ./dwl/patches/ipc.patch
      ./dwl/patches/gaps.patch
      ./dwl/patches/alwayscenter.patch
    ];
  })).override {
    configH = ./dwl/config.def.h;
  };
in
{
  options.my.home.dwl = {
    enable = mkEnableOption "dwl home options";
  };

  config = mkIf cfg.enable {
    my.home = {
      anyrun.enable = true;
      waybar = {
        enable = true;
        theme = "dwl";
      };
      wayland.enable = true;
    };

    home.packages = with pkgs; [
      brillo
      dwl
      dwl-status
      dwlb
      networkmanagerapplet
      playerctl
      procps
      udiskie
      wmenu
    ];
  };
}
