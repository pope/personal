{
  config,
  lib,
  pkgs,
  ...
}:

let
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

  dwl =
    (pkgs.dwl.overrideAttrs (_oldAttrs: {
      patches = [
        ./dwl/patches/ipc.patch
        ./dwl/patches/gaps.patch
        ./dwl/patches/alwayscenter.patch
        ./dwl/patches/movestack.patch
      ];
    })).override
      {
        configH = pkgs.replaceVars ./dwl/config.def.h (
          with pkgs;
          let
            inherit (lib) getExe getExe';
            color = config.my.home.theme.colors.withHex;
          in
          {
            inherit (cfg) dpiScale;
            # apps
            brillo = getExe brillo;
            pamixer = getExe pamixer;
            pkill = getExe' procps "pkill";
            rofi = getExe config.programs.rofi.finalPackage;
            terminal = getExe cfg.terminalPackage;
            uwsm = getExe uwsm;
            wlogout = getExe wlogout;

            # colors
            inherit (color) base00;
            inherit (color) base01;
            inherit (color) base02;
            inherit (color) base09;
            inherit (color) base0E;
          }
        );
      };
in
{
  options.my.home.dwl = {
    enable = lib.mkEnableOption "dwl home options";
    dpiScale = lib.mkOption {
      type = lib.types.int;
      default = 1;
      description = lib.mkDoc ''
        The default DPI scale to use.
      '';
    };
    terminalPackage = lib.mkOption {
      type = lib.types.package;
      default = pkgs.foot;
      defaultText = lib.literalExpression "pkgs.foot";
      description = lib.mkDoc ''
        The package to use when opening a terminal with <SUPER-Enter>.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
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
