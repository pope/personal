{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkOption types;
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
      ./dwl/patches/movestack.patch
    ];
  })).override {
    configH = pkgs.substituteAll (with pkgs;
      let
        inherit (lib) getExe;
        color = config.my.home.theme.colors.withHex;
      in
      {
        inherit (cfg) dpiScale;

        src = ./dwl/config.def.h;

        # apps
        anyrun = getExe config.programs.anyrun.package;
        brillo = getExe brillo;
        foot = getExe foot;
        pamixer = getExe pamixer;
        pkill = "${procps}/bin/pkill";
        rofi = getExe config.programs.rofi.finalPackage;
        uwsm = getExe uwsm;
        wlogout = getExe wlogout;

        # colors
        inherit (color) base00;
        inherit (color) base01;
        inherit (color) base02;
        inherit (color) base03;
        inherit (color) base04;
        inherit (color) base05;
        inherit (color) base06;
        inherit (color) base07;
        inherit (color) base08;
        inherit (color) base09;
        inherit (color) base0A;
        inherit (color) base0B;
        inherit (color) base0C;
        inherit (color) base0D;
        inherit (color) base0E;
        inherit (color) base0F;
      });
  };
in
{
  options.my.home.dwl = {
    enable = mkEnableOption "dwl home options";
    dpiScale = mkOption {
      type = types.int;
      default = 1;
      description = lib.mkDoc ''
        The default DPI scale to use.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      brillo
      dwl
      dwl-status
      dwlb
      networkmanagerapplet
      procps
      wmenu
    ];
  };
}
